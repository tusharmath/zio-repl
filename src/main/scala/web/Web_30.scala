package web

import zio._
import zio.stream._
import zio.clock.Clock
import javax.xml.crypto.Data

object Web_30 {
  type Endpoint = (Method, URL)

  type SocketMiddleware

  sealed trait Status
  object Status {
    case object OK                  extends Status
    case object PAGE_NOT_FOUND      extends Status
    case object UNAUTHORIZED        extends Status
    case object SERVICE_UNAVAILABLE extends Status
  }

  sealed trait Header
  object Header {
    final case class ContentLength(len: Long)             extends Header
    final case class ContentType(value: ContentTypeValue) extends Header
    final case class X(name: String, value: String)       extends Header
    sealed trait ContentTypeValue
    object ContentTypeValue {
      case object PlainText       extends ContentTypeValue
      case object ApplicationJSON extends ContentTypeValue
    }
  }

  // REQUEST
  final case class Request(endpoint: Endpoint, data: Request.Data)
  object Request {
    final case class Data(headers: List[Header], content: Content)
    sealed trait Content extends Product with Serializable
    object Content {
      case object Empty                                              extends Content
      final case class Complete(bytes: ByteBuf)                      extends Content
      final case class Chunked(data: ZStream[Any, Nothing, ByteBuf]) extends Content
    }
  }

  // RESPONSE
  sealed trait Response extends Product with Serializable { self =>
    def withHeader(header: Header): Response =
      self match {
        case Response.Http(status, headers, content) =>
          Response.Http(status, header :: headers, content)
        case Response.Socket(url, socket)            => self
      }

    def updateContentLength: Response =
      self match {
        case m @ Response.Http(_, _, Response.Content.Complete(body)) =>
          m.copy(headers = Header.ContentLength(body.readableBytes) :: m.headers)
        case m                                                        => m
      }

    def updateContentLength(len: Long): Response =
      self match {
        case m @ Response.Http(_, _, Response.Content.Complete(_)) =>
          m.copy(headers = Header.ContentLength(len) :: m.headers)
        case m                                                     => m
      }
  }
  object Response {
    final case class Http(status: Status, headers: List[Header] = Nil, content: Content = Content.Empty)
        extends Response

    final case class Socket(url: URL, socket: SocketMiddleware) extends Response

    sealed trait Content extends Product with Serializable
    object Content {
      case object Empty                                              extends Content
      final case class Complete(bytes: ByteBuf)                      extends Content
      final case class Chunked(body: ZStream[Any, Nothing, ByteBuf]) extends Content
    }
  }

  def lift[A, B](pFunc: PartialFunction[A, B]): Function1[A, ZIO[Any, Unit, B]] =
    A => if (pFunc.isDefinedAt(A)) UIO(pFunc(A)) else ZIO.fail(())

  sealed trait Http[-R, +E, -A, +B] extends Product with Serializable { self =>
    def <>[R1 <: R, E1 >: E, A1 <: A, B1 >: B](other: Http[R1, E1, A1, B1]): Http[R1, E1, A1, B1] =
      Http.OrElse(self, other)
    def apply(req: A): ZIO[R, E, B]                                                               = Http.execute(self, req)
  }

  object Http {
    final case class Constant[B](b: B)                                              extends Http[Any, Nothing, Any, B]
    final case class Fail[E](e: E)                                                  extends Http[Any, E, Any, Nothing]
    final case class Run[R, E, A, B](run: A => ZIO[R, E, B])                        extends Http[R, E, A, B]
    final case class FromEffect[R, E, B](b: ZIO[R, E, B])                           extends Http[R, E, Any, B]
    final case class OrElse[R, E, A, B](h1: Http[R, E, A, B], h2: Http[R, E, A, B]) extends Http[R, E, A, B]

    // Operators
    def fail[E](E: E): Http[Any, E, Any, Nothing]                = Http.Fail(E)
    def fromEffect[R, E, B](b: ZIO[R, E, B]): Http[R, E, Any, B] = Http.FromEffect(b)
    def succeed[B](b: B): Http[Any, Nothing, Any, B]             = Http.Constant(b)
    def of[A]: ofHttp[A]                                         = ofHttp()

    private[Http] def execute[R, E, A, B](http: Http[R, E, A, B], a: A): ZIO[R, E, B] =
      http match {
        case Http.Fail(e)        => ZIO.fail(e)
        case Http.Run(run)       => run(a)
        case Http.Constant(b)    => UIO(b)
        case Http.FromEffect(b)  => b
        case Http.OrElse(h1, h2) => h1(a) <> h2(a)
      }

    // Auxiliary
    final case class ofHttp[A](unit: Unit) extends AnyVal {
      def apply[R, E, B](func: A => ZIO[R, E, B]): Http[R, E, A, B] = Http.Run(func)
    }
  }

  type HttpRoute[-R] = Http[R, Unit, Request, Response]
  object HttpRoute {
    def of[R](func: Request => ZIO[R, Unit, Response]): HttpRoute[R] = Http.of(func)
  }

  object Example {
    type Database      = Has[Database.Service]
    type Authenticator = Has[Authenticator.Service]
    object Database      { type Service }
    object Authenticator { type Service }

    def isConnected: ZIO[Database, Nothing, Boolean]                            = ???
    def isAuthenticated(request: Request): ZIO[Authenticator, Nothing, Boolean] = ???

    def health =
      Http.fromEffect(
        isConnected map {
          case true  => Response.Http(Status.OK)
          case false => Response.Http(Status.SERVICE_UNAVAILABLE)
        }
      )

    def notFound = Http.succeed(Response.Http(Status.PAGE_NOT_FOUND))

    def setContentLength[R, E](http: HttpRoute[R]) =
      HttpRoute.of(req => http(req).map(_.updateContentLength))

    def auth[R, E](http: HttpRoute[R]) =
      HttpRoute.of { req =>
        isAuthenticated(req) >>= {
          case true  => http(req)
          case false => UIO(Response.Http(Status.UNAUTHORIZED))
        }
      }

    def setResponseTiming[R, E](http: HttpRoute[R]) =
      HttpRoute.of { req =>
        for {
          start <- clock.nanoTime
          res   <- http(req)
          end   <- clock.nanoTime
        } yield res.withHeader(Header.X("ResponseTime", (end - start).toString()))
      }

    def server =
      setResponseTiming {
        setContentLength {
          auth(health) <> notFound
        }
      }
  }
}
