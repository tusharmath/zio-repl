package web

import zio._
import zio.stream._
import zio.clock.Clock

object Web_18 {
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

  case class Http[-R](run: Request => ZIO[R, Unit, Response]) { self =>
    def <>[R1 <: R](other: Http[R1]): Http[R1] =
      Http[R1] { req =>
        self.run(req) <> other.run(req)
      }

    def @@[R1 <: R](app: HttpMiddleware[R1]): Http[R1] =
      Http(req =>
        for {
          env <- ZIO.access[R1](i => i)
          res <- app.run(self.provide(env)).run(req)
        } yield res
      )

    def provide[R1 <: R](env: R1): Http[Any] = Http(req => run(req).provide(env))
  }

  case class HttpMiddleware[-R](run: Http[Any] => Http[R])

  object Example {
    def health = Http(_ => UIO(Response.Http(Status.OK)))

    def notFound = Http(_ => UIO(Response.Http(Status.PAGE_NOT_FOUND)))

    def setContentLength: HttpMiddleware[Any] =
      HttpMiddleware(app =>
        Http({ req =>
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
        HttpMiddleware { http =>
          Http(req =>
            for {
              auth <- isAuthenticated(req)
              res  <- if (auth) http.run(req) else UIO(Response.Http(Status.UNAUTHORIZED))
            } yield res
          )
        }

    }

    def setResponseTiming: HttpMiddleware[Clock] =
      HttpMiddleware[Clock] { http =>
        Http[Clock](request =>
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
