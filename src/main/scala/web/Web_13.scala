package web

import zio._
import zio.stream._

object Web_13 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

  type SocketMiddleware = Http[Any, WebSocketFrame, WebSocketFrame]

  sealed trait Status
  object Status {
    case object OK             extends Status
    case object PAGE_NOT_FOUND extends Status
  }

  sealed trait Header
  object Header {
    final case class ContentLength(len: Int)              extends Header
    final case class ContentType(value: ContentTypeValue) extends Header
    sealed trait ContentTypeValue
    object ContentTypeValue {
      case object PlainText       extends ContentTypeValue
      case object ApplicationJSON extends ContentTypeValue
    }
  }

  // HTTP
  sealed trait Http[-R, -A, +B] { self =>
    def <>[R1 <: R, A1 <: A, B1 >: B](other: Http[R1, A1, B1]): Http[R1, A1, B1] = Http.OrElse(self, other)
    def >>>[R1 <: R, A1 >: B, B1](other: Http[R1, A1, B1]): Http[R1, A, B1]      = Http.Pipe(self, other)
  }

  object Http {
    // Constructors
    case object Empty                                          extends Http[Any, Any, Nothing]
    final case class Constant[B](b: B)                         extends Http[Any, Any, B]
    final case class Total[A, B](ab: A => B)                   extends Http[Any, A, B]
    final case class TotalM[R, A, B](ab: A => ZIO[R, Unit, B]) extends Http[R, A, B]

    // Operators
    final case class OrElse[R, A, B](a: Http[R, A, B], b: Http[R, A, B])  extends Http[R, A, B]
    final case class Pipe[R, A, B, C](a: Http[R, A, B], b: Http[R, B, C]) extends Http[R, A, C]

    def totalM[R, A, B](ab: A => ZIO[R, Unit, B]) = TotalM(ab)
    def total[A, B](ab: A => B)                   = Total(ab)
    def constant[B](b: B)                         = Constant(b)
    def of[A]                                     = HttpOfM[A](())

    // Constructor Helpers
    final case class HttpOfM[A](U: Unit) extends AnyVal {
      def apply[R, B](pf: PartialFunction[A, ZIO[R, Unit, B]]) =
        Http.TotalM[R, A, B] {
          case a if pf.isDefinedAt(a) => pf.apply(a)
          case _                      => ZIO.fail(())
        }
    }
  }

  // MIDDLEWARE
  type HttpMiddleware[-R, A] = Http[R, A, A]
  object HttpMiddleware {
    def response[A] = ForResponse[A](())
    def request[A]  = ForRequest[A](())

    def responseM[A] = ForResponseM[A](())
    def requestM[A]  = ForRequestM[A](())

    def of[R, A](pf: PartialFunction[A, ZIO[R, Unit, A]]): HttpMiddleware[R, A] =
      Http.of({ case a if pf.isDefinedAt(a) => pf.apply(a) })

    final case class ForResponse[A](u: Unit) extends AnyVal {
      def apply[R](
        pf: PartialFunction[Response[A], Response[A]]
      ): HttpMiddleware[R, Response[A]] =
        Http.total({ case a if pf.isDefinedAt(a) => pf.apply(a) })
    }

    final case class ForRequest[A](u: Unit) extends AnyVal {
      def apply[R](pf: PartialFunction[Request[A], Request[A]]): HttpMiddleware[R, Request[A]] =
        Http.total({ case a if pf.isDefinedAt(a) => pf.apply(a) })
    }

    final case class ForResponseM[A](u: Unit) extends AnyVal {
      def apply[R](
        pf: PartialFunction[Response[A], Response[A]]
      ): HttpMiddleware[R, Response[A]] =
        Http.total({ case a if pf.isDefinedAt(a) => pf.apply(a) })
    }

    final case class ForRequestM[A](u: Unit) extends AnyVal {
      def apply[R](pf: PartialFunction[Request[A], Request[A]]): HttpMiddleware[R, Request[A]] =
        Http.total({ case a if pf.isDefinedAt(a) => pf.apply(a) })
    }
  }

  // ROUTE
  type HttpRoute[-R, A] = Http[R, Request[A], Response[A]]
  object HttpRoute {
    def fromEffect[R, A](pf: PartialFunction[(Route, Request.Data[A]), ZIO[R, Unit, Response[A]]]): HttpRoute[R, A] =
      Http.of[Request[A]] {
        case Request(method -> url, data) =>
          val route = method -> url.path -> data
          if (pf.isDefinedAt(route)) pf.apply(route) else ZIO.fail(())
      }

    def of[R, A](pf: PartialFunction[(Route, Request.Data[A]), Response[A]]): HttpRoute[R, A] =
      Http.of[Request[A]] {
        case Request(method -> url, data) =>
          val route = method -> url.path -> data
          if (pf.isDefinedAt(route)) UIO(pf.apply(route)) else ZIO.fail(())
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
  sealed trait Response[+A] extends Product with Serializable
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

  type HttpApp[R] = HttpRoute[R, ByteBuf]
  final case class Server[R](port: Int, http: HttpApp[R])

  object Example {
    import Method._
    import Path._
    import Response._

    def contentLength =
      HttpMiddleware.response[ByteBuf] {
        case res @ Response.Http(status, headers, Content.Complete(body)) =>
          res.copy(headers = Header.ContentLength(body.readableBytes) :: headers)
      }

    def plainText[A] =
      HttpMiddleware.response[A] {
        case res @ Response.Http(status, headers, Content.Complete(body)) =>
          if (headers.contains(Header.ContentType)) res
          else res.copy(headers = (Header.ContentType(Header.ContentTypeValue.PlainText) :: res.headers))
      }

    def statusCode[A](status: Status): HttpRoute[Any, A] =
      HttpRoute.of {
        case _ => Response.Http(status)
      }

    def notFound = statusCode(Status.PAGE_NOT_FOUND)

    def health[A]: HttpRoute[Any, A] =
      HttpRoute.of { case GET -> Root / "health" -> _ => Response.Http(Status.OK) }

    def app                          = health[ByteBuf] >>> contentLength

  }
}

// - UNSOLVED ISSUES
