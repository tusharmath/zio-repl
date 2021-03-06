package web

import zio._
import zio.stream._
import zio.clock.Clock
import javax.xml.crypto.Data

object Web_22 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

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
    final case class ContentLength(len: Int)              extends Header
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
        case Response.Http(status, headers, content) => Response.Http(status, header :: headers, content)
        case Response.Socket(url, socket)            => self
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

  sealed trait Http[-R] extends Product with Serializable { self =>
    def <>[R1 <: R](other: Http[R1]): Http[R1]                = Http.OrElse(self, other)
    def @@[R1 <: R](middleware: HttpMiddleware[R1]): Http[R1] = Http.Middleware(self, middleware)
  }

  object Http {
    final case class Run[R](run: Request => ZIO[R, Unit, Response])              extends Http[R]
    final case class OrElse[R](http0: Http[R], http1: Http[R])                   extends Http[R]
    final case class Middleware[R](http: Http[R], middleware: HttpMiddleware[R]) extends Http[R]

    def responseM[R](response: ZIO[R, Unit, Response]): Http[R] = Http.Run(_ => response)
    def response(response: Response): Http[Any]                 = Http.Run(_ => UIO(response))
    def status(status: Status): Http[Any]                       = Http.response(Response.Http(status))
    def of[R](func: Request => ZIO[R, Unit, Response]): Http[R] = Http.Run(func)

    def execute[R](http: Http[R], req: Request): ZIO[R, Unit, Response] =
      http match {
        case Http.Run(run)                     => run(req)
        case Http.OrElse(http0, http1)         => Http.execute(http0, req) <> Http.execute(http1, req)
        case Http.Middleware(http, middleware) =>
          middleware match {
            case HttpMiddleware.Constructor(httpCB) => Http.execute(Http.of(Http.execute(http, _)), req)
            case HttpMiddleware.Concat(m1, m2)      => Http.execute(http @@ m1 @@ m2, req)
          }
      }
  }

  sealed trait HttpMiddleware[-R] extends Product with Serializable

  object HttpMiddleware {
    final case class Constructor[R](httpCB: Http[Any] => Http[R])            extends HttpMiddleware[R]
    final case class Concat[R](m1: HttpMiddleware[R], m2: HttpMiddleware[R]) extends HttpMiddleware[R]

    def of[R](cb: (Request, Http[Any]) => ZIO[R, Unit, Response]) =
      HttpMiddleware.Constructor(http => Http.of(req => cb(req, http)))

    def response(pFunc: PartialFunction[Response, Response]): HttpMiddleware[Any] =
      HttpMiddleware.of((req, http) => Http.execute(http, req) >>= lift(pFunc))
  }

  object Example {
    type Database = Database.Service
    object Database {
      type Service
      def isConnected: ZIO[Database, Nothing, Boolean] = ???
    }

    def health =
      Http.responseM(
        Database.isConnected map {
          case true  => Response.Http(Status.OK)
          case false => Response.Http(Status.SERVICE_UNAVAILABLE)
        }
      )

    def notFound = Http.status(Status.PAGE_NOT_FOUND)

    def setContentLength =
      HttpMiddleware.response {
        case m @ Response.Http(_, _, Response.Content.Complete(body)) =>
          m.copy(headers = Header.ContentLength(body.readableBytes) :: m.headers)
      }

    type Authenticator = Has[Authenticator.Service]
    object Authenticator {
      type Service

      def isAuthenticated(request: Request): ZIO[Authenticator, Nothing, Boolean] = ???

      def auth =
        HttpMiddleware.of { (req, http) =>
          isAuthenticated(req) >>= {
            case true  => Http.execute(http, req)
            case false => UIO(Response.Http(Status.UNAUTHORIZED))
          }
        }
    }

    def setResponseTiming =
      HttpMiddleware.of { (req, http) =>
        for {
          start <- clock.nanoTime
          res   <- Http.execute(http, req)
          end   <- clock.nanoTime
        } yield res.withHeader(Header.X("ResponseTime", (end - start).toString()))
      }

    def server = health @@ Authenticator.auth <> notFound @@ setResponseTiming @@ setContentLength
  }
}

// - UNSOLVED ISSUES
