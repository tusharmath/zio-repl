package web

import zio._
import zio.stream._

// Generalize IO and Stream
object Web_11 {
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

  case class Http[-A, +B](ab: A => Http.Next[B]) { self =>
    def <>[A1 <: A, B1 >: B](other: Http[A1, B1]): Http[A1, B1] =
      Http(a => self.ab(a) <> (other.ab(a)))

    def >>>[A1 >: B, B1](other: Http[A1, B1]): Http[A, B1] =
      Http(a => self.ab(a) >>= other.ab)
  }

  object Http {

    sealed trait Next[+A] { self =>
      def <>[A1 >: A](other: Next[A1]): Next[A1] = ???
      def >>=[B](other: A => Next[B]): Next[B]   = ???
    }

    object Next {
      case object Empty                                   extends Next[Nothing]
      case class Constant[B](next: B)                     extends Next[B]
      case class Once[B](next: IO[Unit, B])               extends Next[B]
      case class Multiple[B](next: ZStream[Any, Unit, B]) extends Next[B]
    }

    def once[A, B](pf: PartialFunction[A, IO[Unit, B]]): Http[A, B] =
      Http(a => if (pf.isDefinedAt(a)) Next.Once(pf.apply(a)) else Next.Once(ZIO.fail(())))

    def multiple[A, B](pf: PartialFunction[A, ZStream[Any, Unit, B]]): Http[A, B] =
      Http(a => if (pf.isDefinedAt(a)) Next.Multiple(pf.apply(a)) else Next.Multiple(ZStream.fail(())))
  }

  type HttpMiddleware[A] = Http[A, A]
  object HttpMiddleware {
    def of[A](pf: PartialFunction[A, IO[Unit, A]]): HttpMiddleware[A]                                     =
      Http.once({ case a if pf.isDefinedAt(a) => pf.apply(a) })

    def request[R](pf: PartialFunction[Request[R], IO[Unit, Request[R]]]): HttpMiddleware[Request[R]]     =
      Http.once({ case a if pf.isDefinedAt(a) => pf.apply(a) })

    def response[R](pf: PartialFunction[Response[R], IO[Unit, Response[R]]]): HttpMiddleware[Response[R]] =
      Http.once({ case a if pf.isDefinedAt(a) => pf.apply(a) })
  }

  type SocketMiddleware[A] = Http[A, A]
  object SocketMiddleware {
    def of[A](pf: PartialFunction[A, ZStream[Any, Unit, A]]): HttpMiddleware[A]                                     =
      Http.multiple({ case a if pf.isDefinedAt(a) => pf.apply(a) })

    def request[R](pf: PartialFunction[Request[R], ZStream[Any, Unit, Request[R]]]): HttpMiddleware[Request[R]]     =
      Http.multiple({ case a if pf.isDefinedAt(a) => pf.apply(a) })

    def response[R](pf: PartialFunction[Response[R], ZStream[Any, Unit, Response[R]]]): HttpMiddleware[Response[R]] =
      Http.multiple({ case a if pf.isDefinedAt(a) => pf.apply(a) })
  }

  object HttpRoute {
    def of[R](pf: PartialFunction[(Route, Request.Data[R]), IO[Unit, Response[R]]]): HttpRoute[R] =
      Http.once {
        case Request(method -> url, data) =>
          val route = method -> url.path -> data
          if (pf.isDefinedAt(route)) pf.apply(route) else ZIO.fail(())
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
// - Env in HTTP middleware
