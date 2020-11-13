package web

import zio._
import zio.stream._
import zio.clock.Clock

object Web_17 {
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

  case class Http[+X](run: Request[Nothing] => IO[Unit, Response[X]]) { self =>
    def <>[X1 >: X](other: Http[X1]): Http[X1] =
      Http[X1] { req =>
        self.run(req) <> other.run(req)
      }

    def @@[X1 >: X](app: HttpMiddleware[X1]): Http[X1] = ???
  }

  type HttpMiddleware[+X]
}

// - UNSOLVED ISSUES
