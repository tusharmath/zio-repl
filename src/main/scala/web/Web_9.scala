package web

import zio._
import zio.stream._

// Without the data polymorphism in Request/Response
object Web_9 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

  type HttpRoute[R] = Http[Request[R], Response[R]]
  type HttpApp[R]   = HttpRoute[R]

  sealed trait Status
  object Status {
    case object OK             extends Status
    case object PAGE_NOT_FOUND extends Status
  }

  sealed trait Header
  object Header {
    case class ContentLength(len: Int)              extends Header
    case class ContentType(value: ContentTypeValue) extends Header
    sealed trait ContentTypeValue
    object ContentTypeValue {
      case object PlainText       extends ContentTypeValue
      case object ApplicationJSON extends ContentTypeValue
    }
  }

  case class Http[-A, +B](ab: A => IO[Unit, B]) { self =>
    def <>[A1 <: A, B1 >: B](other: Http[A1, B1]): Http[A1, B1] =
      Http(a => self.ab(a).orElse(other.ab(a)))

    def >>>[A1 >: B, B1](other: Http[A1, B1]): Http[A, B1] =
      Http(a => self.ab(a) >>= other.ab)
  }

  object Http {
    def of[A, B](pf: PartialFunction[A, IO[Unit, B]]): Http[A, B] =
      Http(a => if (pf.isDefinedAt(a)) pf.apply(a) else ZIO.fail(()))
  }

  type HttpMiddleware[A] = Http[A, A]
  object HttpMiddleware {
    def of[A](pf: PartialFunction[A, IO[Unit, A]]): HttpMiddleware[A] =
      Http(a => if (pf.isDefinedAt(a)) pf.apply(a) else ZIO.fail(()))

    def request[R](pf: PartialFunction[Request[R], IO[Unit, Request[R]]]): HttpMiddleware[Request[R]] =
      Http(req => if (pf.isDefinedAt(req)) pf.apply(req) else ZIO.fail(()))

    def response[R](pf: PartialFunction[Response[R], IO[Unit, Response[R]]]): HttpMiddleware[Response[R]] =
      Http(a => if (pf.isDefinedAt(a)) pf.apply(a) else ZIO.fail(()))
  }

  object HttpRoute {
    def of[R](pf: PartialFunction[(Route, Request.Data[R]), IO[Unit, Response[R]]]): HttpRoute[R] =
      Http {
        case Request(method -> url, data) =>
          val route = method -> url.path -> data
          if (pf.isDefinedAt(route)) pf.apply(route) else ZIO.fail(())
        case _                            => ZIO.fail(())
      }
  }

  // ABS
  case class Request[-R](endpoint: Endpoint, data: Request.Data[R])

  object Request {
    case class Data[-R](headers: List[Header], content: Content[R])
    sealed trait Content[-R]
    object Content {
      case object Empty                                      extends Content[Any]
      case class Complete[A](data: A)                        extends Content[Any]
      case class Chunked[R, A](data: ZStream[R, Nothing, A]) extends Content[R]
    }
  }

  sealed trait Response[-R] extends Product with Serializable
  object Response {
    case object Empty                                     extends Response[Any]
    case class Data[R](status: Status, headers: List[Header] = Nil, content: Content[R] = Content.Empty)
        extends Response[R]
    case class Socket[R](middleware: SocketMiddleware[R]) extends Response[R]

    sealed trait Content[-R] extends Product with Serializable
    object Content {
      case object Empty                                         extends Content[Any]
      case class Complete(body: ByteBuf)                        extends Content[Any]
      case class Chunked[R](body: ZStream[R, Nothing, ByteBuf]) extends Content[R]
    }
  }

  case class Server[A, B](port: Int, http: Http[A, B])

  object Example {
    import Method._
    import Path._
    import Response._

    def contentLength =
      HttpMiddleware.response[Any] {
        case res @ Data(status, headers, Content.Complete(body)) =>
          UIO(res.copy(headers = Header.ContentLength(body.readableBytes) :: headers))
      }

    def plainText =
      HttpMiddleware.response[Any] {
        case res @ Data(status, headers, Content.Complete(body)) =>
          UIO {
            if (headers.contains(Header.ContentType)) res
            else res.copy(headers = (Header.ContentType(Header.ContentTypeValue.PlainText) :: res.headers))
          }
      }

    def statusCode[A](status: Status): HttpRoute[Any] =
      HttpRoute.of {
        case _ => UIO(Response.Data(status))
      }
    def notFound                                      = statusCode(Status.PAGE_NOT_FOUND)

    def health: HttpRoute[Any] =
      HttpRoute.of({ case GET -> Root / "health" -> _ => UIO(Response.Data(Status.OK)) })

    def app                    = health >>> contentLength

  }
}

// - UNSOLVED ISSUES
// - Common Middleware API between web-sockets and HTTP
