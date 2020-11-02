package web

package web

import zio._
import zio.stream._
import izumi.reflect.macrortti.LightTypeTagRef.Boundaries.Empty

// Focus on middleware API
object Web_6 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

  sealed trait Status
  object Status {
    case object OK             extends Status
    case object PAGE_NOT_FOUND extends Status
  }

  case class Header(name: String, value: String)

  case class Http[-A, +B](ab: Request[A] => IO[Unit, Response[B]]) { self =>
    def <>[A1 <: A, B1 >: B](other: Http[A1, B1]): Http[A1, B1] = Http(req => self.ab(req).orElse(other.ab(req)))

    def mapM[C](bc: Response[B] => IO[Unit, Response[C]]): Http[A, C]     =
      Http.of { case req => self.ab(req) >>= bc }

    def contramapM[X](bc: Request[X] => IO[Unit, Request[A]]): Http[X, B] =
      Http.of { case req => bc(req) >>= self.ab }
  }

  object Http {
    def of[A, B](pf: PartialFunction[Request[A], IO[Unit, Response[B]]]): Http[A, B] =
      Http(req => if (pf.isDefinedAt(req)) pf.apply(req) else ZIO.fail(()))

    def route[A, B](pf: PartialFunction[(Route, Request.Data[A]), IO[Unit, Response[B]]]): Http[A, B] =
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

    def contentLength(http: Http[ByteBuf, ByteBuf]): Http[ByteBuf, ByteBuf] =
      http mapM {
        case m @ Basic(status, headers, Content.Complete(body)) =>
          UIO(m.copy(headers = Header("Content-Length", body.readableBytes.toString()) :: headers))

        case m                                                  => UIO(m)
      }

    def health: Http[ByteBuf, ByteBuf] =
      Http route {
        case GET -> _ / "health" -> _ => UIO(Response.Basic(Status.OK))
      }
  }
}
