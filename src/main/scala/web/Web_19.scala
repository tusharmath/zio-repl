package web

import zio._
import zio.stream._
import zio.clock.Clock

object Web_19 {
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

  sealed trait Http[-R] { self =>
    def <>[R1 <: R](other: Http[R1]): Http[R1]                = Http.OrElse(self, other)
    def @@[R1 <: R](middleware: HttpMiddleware[R1]): Http[R1] = Http.Middleware(self, middleware)

    def run(request: Request): ZIO[R, Unit, Response] = Http.run(self)(request)
  }

  object Http {
    // ADT Constructors
    case object Empty                                             extends Http[Any]
    case class Constant(response: Response)                       extends Http[Any]
    case class EffectCB[R](cb: Request => ZIO[R, Unit, Response]) extends Http[R]

    // ADT Operators
    case class OrElse[R](a: Http[R], b: Http[R])                           extends Http[R]
    case class Middleware[R](http: Http[R], middleware: HttpMiddleware[R]) extends Http[R]

    // Helpers
    def response(response: Response): Http[Any] = Http.Constant(response)

    def of(pFunc: PartialFunction[Request, Response]): Http[Any] =
      Http.EffectCB(req => if (pFunc.isDefinedAt(req)) UIO(pFunc(req)) else ZIO.fail(()))

    def lift[R](func: Request => ZIO[R, Unit, Response]): Http[R] =
      Http.EffectCB(func)

    // Interpreter
    def run[R](http: Http[R])(req: Request): ZIO[R, Unit, Response] =
      http match {
        case Http.Empty                        => ZIO.fail(())
        case Http.Constant(response)           => UIO(response)
        case Http.EffectCB(cb)                 => cb(req)
        case Http.OrElse(a, b)                 => run(a)(req) <> run(b)(req)
        case Http.Middleware(http, middleware) =>
          ZIO.environment[R] >>= { env =>
            run(middleware.run(Http.lift(run(http)(_).provide(env))))(req)
          }
      }
  }

  case class HttpMiddleware[-R] private (run: Http[Any] => Http[R])
  object HttpMiddleware {
    def of[R](func: Http[Any] => Http[R]): HttpMiddleware[R] = HttpMiddleware(func)
  }

  object Example {
    def health   = Http.response(Response.Http(Status.OK))
    def notFound = Http.response(Response.Http(Status.PAGE_NOT_FOUND))

    def setContentLength: HttpMiddleware[Any] =
      HttpMiddleware.of(app =>
        Http.lift({ req =>
          app.run(req) map {
            case m @ Response.Http(_, _, Response.Content.Complete(body)) =>
              m.copy(headers = Header.ContentLength(body.readableBytes) :: m.headers)
            case m                                                        => m
          }
        })
      )

    type Authenticator = Has[Authenticator.Service]
    object Authenticator {
      type Service

      def isAuthenticated(request: Request): ZIO[Authenticator, Nothing, Boolean] = ???

      def auth =
        HttpMiddleware.of { http =>
          Http.lift(req =>
            for {
              auth <- isAuthenticated(req)
              res  <- if (auth) http.run(req) else UIO(Response.Http(Status.UNAUTHORIZED))
            } yield res
          )
        }

    }

    def setResponseTiming: HttpMiddleware[Clock] =
      HttpMiddleware.of { http =>
        Http.lift(request =>
          for {
            start <- clock.nanoTime
            res0  <- http.run(request)
            end   <- clock.nanoTime
          } yield res0.withHeader(Header.X("ResponseTime", (end - start).toString()))
        )
      }

    def server = health @@ Authenticator.auth <> notFound @@ setResponseTiming @@ setContentLength
  }
}

// - UNSOLVED ISSUES
