package web

import zio._
import zio.stream._
import zio.clock.Clock

// Declarative Encoding
object Web_15 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

  type SocketMiddleware

  sealed trait Status
  object Status {
    case object OK             extends Status
    case object PAGE_NOT_FOUND extends Status
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

  // HTTP (Http4S way)
  case class Http[-R](run: Request[ByteBuf] => ZIO[R, Unit, Response[ByteBuf]]) { self =>
    def <>[R1 <: R](other: Http[R1]): Http[R1] =
      Http { req =>
        self.run(req) <> other.run(req)
      }

    def @@[R1 <: R](app: HttpMiddleware[R1]): Http[R1] =
      app.make(self)
  }

  case class HttpMiddleware[R](make: Http[R] => Http[R])
  object HttpMiddleware {
    def response[R](cb: PartialFunction[Response[ByteBuf], Response[ByteBuf]]): HttpMiddleware[R] =
      HttpMiddleware { http =>
        Http(request => http.run(request) >>= { res => if (cb.isDefinedAt(res)) UIO(cb(res)) else ZIO.fail(()) })
      }

    def responseM[R](cb: Response[ByteBuf] => ZIO[R, Unit, Response[ByteBuf]]): HttpMiddleware[R] =
      HttpMiddleware { http =>
        Http(request => http.run(request) >>= cb)
      }

    def request[R](cb: Request[ByteBuf] => Request[ByteBuf]): HttpMiddleware[R] =
      HttpMiddleware { http =>
        Http(request => http.run(cb(request)))
      }

    def requestM[R](cb: Request[ByteBuf] => ZIO[R, Unit, Request[ByteBuf]]): HttpMiddleware[R] =
      HttpMiddleware { http =>
        Http(request => cb(request) >>= http.run)
      }
  }

  // REQUEST
  final case class Request[+A](endpoint: Endpoint, data: Request.Data[A])
  object Request {
    final case class Data[+A](headers: List[Header], content: Content[A])
    sealed trait Content[+A] extends Product with Serializable
    object Content {
      case object Empty                                           extends Content[Nothing]
      final case class Complete[A](data: A)                       extends Content[A]
      final case class Chunked[A](data: ZStream[Any, Nothing, A]) extends Content[A]
    }
  }

  // RESPONSE
  sealed trait Response[+A] extends Product with Serializable { self =>
    def withHeader(header: Header): Response[A] =
      self match {
        case Response.Http(status, headers, content) => Response.Http(status, header :: headers, content)
        case Response.Socket(url, socket)            => self
      }
  }
  object Response {
    final case class Http[+A](status: Status, headers: List[Header] = Nil, content: Content[A] = Content.Empty)
        extends Response[A]

    final case class Socket(url: URL, socket: SocketMiddleware) extends Response[Nothing]

    sealed trait Content[+A] extends Product with Serializable
    object Content {
      case object Empty                                           extends Content[Nothing]
      final case class Complete[A](body: A)                       extends Content[A]
      final case class Chunked[A](body: ZStream[Any, Nothing, A]) extends Content[A]
    }
  }

  object Example {
    def health   = Http(_ => UIO(Response.Http(Status.OK)))
    def notFound = Http(_ => UIO(Response.Http(Status.PAGE_NOT_FOUND)))

    def setContentLength[R] =
      HttpMiddleware.response[R] {
        case m @ Response.Http(_, _, Response.Content.Complete(body)) =>
          m.copy(headers = Header.ContentLength(body.readableBytes) :: m.headers)
      }

    def setResponseTiming[R]: HttpMiddleware[R with Clock] =
      HttpMiddleware { other =>
        Http { req =>
          for {
            start <- clock.nanoTime
            res0  <- other.run(req)
            end   <- clock.nanoTime
          } yield res0.withHeader(Header.X("ResponseTime", (end - start).toString()))
        }
      }

    def server = health <> notFound @@ setContentLength @@ setResponseTiming
  }
}

// - UNSOLVED ISSUES
