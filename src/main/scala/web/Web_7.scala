package web

import zio._
import zio.stream._

// Focus on middleware API
object Web_7 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

  sealed trait Status
  object Status {
    case object OK             extends Status
    case object PAGE_NOT_FOUND extends Status
  }

  case class Header(name: String, value: String)

  case class Http[-A, +B](ab: A => IO[Unit, B]) { self =>
    def <>[A1 <: A, B1 >: B](other: Http[A1, B1]): Http[A1, B1] =
      Http(req => self.ab(req).orElse(other.ab(req)))

    def >>>[A1 >: B, B1](other: Http[A1, B1]): Http[A, B1] =
      Http(req => self.ab(req) >>= other.ab)
  }

  object Http {
    def of[A, B](pf: PartialFunction[A, IO[Unit, B]]): Http[A, B] =
      Http(req => if (pf.isDefinedAt(req)) pf.apply(req) else ZIO.fail(()))
  }

  type HttpRoute[-A, +B] = Http[Request[A], Response[B]]
  object HttpRoute {
    def of[A, B](pf: PartialFunction[(Route, Request.Data[A]), IO[Unit, Response[B]]]): HttpRoute[A, B] =
      Http.of {
        case Request(method -> url, data) =>
          val route = method -> url.path -> data
          if (pf.isDefinedAt(route)) pf.apply(route) else ZIO.fail(())
      }
  }

  case class Request[+A](endpoint: Endpoint, data: Request.Data[A])
  object Request {
    case class Data[+A](headers: List[Header], content: Content[A])
    sealed trait Content[+A]
    object Content {
      case object Empty                                     extends Content[Nothing]
      case class Complete[A](data: A)                       extends Content[A]
      case class Chunked[A](data: ZStream[Any, Nothing, A]) extends Content[A]
    }
  }

  sealed trait Response[+A]

  object Response {
    case class Basic[A](status: Status, headers: List[Header] = Nil, content: Content[A] = Content.Empty)
        extends Response[A]
    case class Socket(webSocketMiddleware: SocketMiddleware[Any]) extends Response[Nothing]

    sealed trait Content[+A]
    object Content {
      case object Empty                                     extends Content[Nothing]
      case class Complete[A](body: A)                       extends Content[A]
      case class Chunked[A](body: ZStream[Any, Nothing, A]) extends Content[A]
    }
  }

  case class Server[A, B](port: Int, http: Http[A, B])

  object Example {
    import Method._
    import Path._
    import Response._

    def contentLength =
      Http.of[Response[ByteBuf], Response[ByteBuf]] {
        case m @ Basic(status, headers, Content.Complete(body)) =>
          UIO(m.copy(headers = Header("Content-Length", body.readableBytes.toString()) :: headers))
      }

    def health: HttpRoute[ByteBuf, ByteBuf] =
      HttpRoute.of { case GET -> _ / "health" -> _ => UIO(Response.Basic(Status.OK)) }

    def healthWithContentLength             = health >>> contentLength
  }
}
