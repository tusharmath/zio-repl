package web

import zio._
import zio.stream._
import zio.clock.Clock

object Web_27 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

  type SocketMiddleware

  sealed trait Status
  object Status {
    case object OK             extends Status
    case object PAGE_NOT_FOUND extends Status
    case object UNAUTHORIZED   extends Status
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

  final case class Http[-R](private val run: Request => ZIO[R, Unit, Response]) extends AnyVal { self =>
    def apply(req: Request): ZIO[R, Unit, Response] = self.run(req)

    def <>[R1 <: R](other: Http[R1]): Http[R1]                = Http(req => self(req) <> other(req))
    def @@[R1 <: R](middleware: HttpMiddleware[R1]): Http[R1] = Http(middleware(self)(_))
    def provide[R1 <: R](env: R1): Http[Any]                  = Http(self(_).provide(env))
  }

  object Http {
    def response(response: Response): Http[Any] = Http(_ => UIO(response))
  }

  case class HttpMiddleware[-R](private val run: Http[Any] => Http[R]) { self =>
    def apply[R1 <: R](http: Http[R1]): Http[R1] =
      Http(req => ZIO.environment[R1] >>= { env => run(http.provide(env))(req) })
  }

  object HttpMiddleware {
    def of[R](cb: (Request, Http[Any]) => ZIO[R, Unit, Response]) =
      HttpMiddleware(http => Http(req => cb(req, http)))

    def response(pFunc: PartialFunction[Response, Response]): HttpMiddleware[Any] =
      HttpMiddleware(http => Http(http(_) >>= lift(pFunc)))
  }

  object Example {
    def health   = Http.response(Response.Http(Status.OK))
    def notFound = Http.response(Response.Http(Status.PAGE_NOT_FOUND))

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
            case true  => http(req)
            case false => UIO(Response.Http(Status.UNAUTHORIZED))
          }
        }
    }

    def setResponseTiming =
      HttpMiddleware.of { (req, http) =>
        for {
          start <- clock.nanoTime
          res   <- http(req)
          end   <- clock.nanoTime
        } yield res.withHeader(Header.X("ResponseTime", (end - start).toString()))
      }

    def server = health @@ Authenticator.auth <> notFound @@ setResponseTiming @@ setContentLength
  }
}

// - UNSOLVED ISSUES
