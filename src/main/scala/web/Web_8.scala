package web

import zio._
import zio.stream._

// Focus on middleware API
object Web_8 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

  type HttpRoute[R, A] = Http[Request[R, A], Response[R, A]]
  type HttpApp[R]      = HttpRoute[R, ByteBuf]

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

    def request[R, A](pf: PartialFunction[Request[R, A], IO[Unit, Request[R, A]]]): HttpMiddleware[Request[R, A]] =
      Http(req => if (pf.isDefinedAt(req)) pf.apply(req) else ZIO.fail(()))

    def response[R, A](pf: PartialFunction[Response[R, A], IO[Unit, Response[R, A]]]): HttpMiddleware[Response[R, A]] =
      Http(a => if (pf.isDefinedAt(a)) pf.apply(a) else ZIO.fail(()))
  }

  object HttpRoute {
    def of[R, A](pf: PartialFunction[(Route, Request.Data[R, A]), IO[Unit, Response[R, A]]]): HttpRoute[R, A] =
      Http {
        case Request(method -> url, data) =>
          val route = method -> url.path -> data
          if (pf.isDefinedAt(route)) pf.apply(route) else ZIO.fail(())
        case _                            => ZIO.fail(())
      }
  }

  // ABS
  case class Request[-R, +A](endpoint: Endpoint, data: Request.Data[R, A])

  object Request {
    case class Data[-R, +A](headers: List[Header], content: Content[R, A])
    sealed trait Content[-R, +A]
    object Content {
      case object Empty                                      extends Content[Any, Nothing]
      case class Complete[A](data: A)                        extends Content[Any, A]
      case class Chunked[R, A](data: ZStream[R, Nothing, A]) extends Content[R, A]
    }
  }

  sealed trait Response[-R, +A] extends Product with Serializable
  object Response {
    case object Empty                                     extends Response[Any, Nothing]
    case class Data[R, A](status: Status, headers: List[Header] = Nil, content: Content[R, A] = Content.Empty)
        extends Response[R, A]
    case class Socket[R](middleware: SocketMiddleware[R]) extends Response[R, Nothing]

    sealed trait Content[-R, +A] extends Product with Serializable
    object Content {
      case object Empty                                      extends Content[Any, Nothing]
      case class Complete[A](body: A)                        extends Content[Any, A]
      case class Chunked[R, A](body: ZStream[R, Nothing, A]) extends Content[R, A]
    }
  }

  case class Server[A, B](port: Int, http: Http[A, B])

  object Example {
    import Method._
    import Path._
    import Response._

    def contentLength =
      HttpMiddleware.response[Any, ByteBuf] {
        case res @ Data(status, headers, Content.Complete(body)) =>
          UIO(res.copy(headers = Header.ContentLength(body.readableBytes) :: headers))
      }

    def plainText =
      HttpMiddleware.response[Any, ByteBuf] {
        case res @ Data(status, headers, Content.Complete(body)) =>
          UIO {
            if (headers.contains(Header.ContentType)) res
            else res.copy(headers = (Header.ContentType(Header.ContentTypeValue.PlainText) :: res.headers))
          }
      }

    def statusCode[A](status: Status): HttpRoute[Any, A] =
      HttpRoute.of {
        case _ => UIO(Response.Data(status))
      }
    def notFound                                         = statusCode(Status.PAGE_NOT_FOUND)

    def health: HttpRoute[Any, ByteBuf] =
      HttpRoute.of({ case GET -> Root / "health" -> _ => UIO(Response.Data(Status.OK)) })

    def app                             = health >>> contentLength

  }
}
