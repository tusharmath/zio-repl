package web

import zio._
import zio.stream._

object Web_12 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

  type SocketMiddleware[R]

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
    case object Empty                                                     extends Http[Any, Any, Nothing]
    final case class Constant[B](b: B)                                    extends Http[Any, Any, B]
    final case class FromEffect[R, A, B](ab: A => ZIO[R, Unit, B])        extends Http[R, A, B]
    final case class Socket[R](url: URL, middleware: SocketMiddleware[R]) extends Http[R, Any, Nothing]

    // Operators
    final case class OrElse[R, A, B](a: Http[R, A, B], b: Http[R, A, B])  extends Http[R, A, B]
    final case class Pipe[R, A, B, C](a: Http[R, A, B], b: Http[R, B, C]) extends Http[R, A, C]

    def of[A]                                         = HttpOf[A](())
    def fromEffect[R, A, B](ab: A => ZIO[R, Unit, B]) = FromEffect(ab)
    def constant[B](b: B)                             = Constant(b)

    // Constructor Helpers
    final case class HttpOf[A](U: Unit) extends AnyVal {
      def apply[R, B](pf: PartialFunction[A, ZIO[R, Unit, B]]) =
        Http.FromEffect[R, A, B]({
          case a if pf.isDefinedAt(a) => pf.apply(a)
          case _                      => ZIO.fail(())
        })
    }
  }

  // MIDDLEWARE
  type HttpMiddleware[R, A] = Http[R, A, A]
  object HttpMiddleware {
    def of[R, A](pf: PartialFunction[A, ZIO[R, Unit, A]]): HttpMiddleware[R, A] =
      Http.fromEffect({ case a if pf.isDefinedAt(a) => pf.apply(a) })

    def response[A]                                                             = ForResponse[A](())
    def request[A]                                                              = ForRequest[A](())

    final case class ForResponse[A](u: Unit) extends AnyVal {
      def apply[R](
        pf: PartialFunction[Response[R, A], ZIO[R, Unit, Response[R, A]]]
      ): HttpMiddleware[R, Response[R, A]] =
        Http.fromEffect({ case a if pf.isDefinedAt(a) => pf.apply(a) })
    }

    final case class ForRequest[A](u: Unit) extends AnyVal {
      def apply[R](pf: PartialFunction[Request[R, A], ZIO[R, Unit, Request[R, A]]]): HttpMiddleware[R, Request[R, A]] =
        Http.fromEffect({ case a if pf.isDefinedAt(a) => pf.apply(a) })
    }
  }

  // ROUTE
  type HttpRoute[R, A] = Http[R, Request[R, A], Response[R, A]]
  object HttpRoute {
    def of[R, A](
      pf: PartialFunction[(Route, Request.Data[R, A]), IO[Unit, Response[R, A]]]
    ): HttpRoute[R, A] =
      Http.of[Request[R, A]] {
        case Request(method -> url, data) =>
          val route = method -> url.path -> data
          if (pf.isDefinedAt(route)) pf.apply(route) else ZIO.fail(())
      }
  }

  // REQUEST
  final case class Request[-R, +A](endpoint: Endpoint, data: Request.Data[R, A])

  object Request {
    final case class Data[-R, +A](headers: List[Header], content: Content[R, A])
    sealed trait Content[-R, +A]
    object Content {
      case object Empty                                            extends Content[Any, Nothing]
      final case class Complete[A](data: A)                        extends Content[Any, A]
      final case class Chunked[R, A](data: ZStream[R, Nothing, A]) extends Content[R, A]
    }
  }

  sealed trait Response[-R, +A] extends Product with Serializable
  object Response {
    case object Empty extends Response[Any, Nothing]
    final case class Data[R, A](status: Status, headers: List[Header] = Nil, content: Content[R, A] = Content.Empty)
        extends Response[R, A]

    sealed trait Content[-R, +A] extends Product with Serializable
    object Content {
      case object Empty                                            extends Content[Any, Nothing]
      final case class Complete[A](body: A)                        extends Content[Any, A]
      final case class Chunked[R, A](body: ZStream[R, Nothing, A]) extends Content[R, A]
    }
  }

  final case class Server[R, A, B](port: Int, http: Http[R, A, B])

  object Example {
    import Method._
    import Path._
    import Response._

    def contentLength =
      HttpMiddleware.of[Any, Response[Any, ByteBuf]] {
        case res @ Data(status, headers, Content.Complete(body)) =>
          UIO(res.copy(headers = Header.ContentLength(body.readableBytes) :: headers))
      }

    def plainText[A] =
      HttpMiddleware.of[Any, Response[Any, A]] {
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

    def notFound = statusCode(Status.PAGE_NOT_FOUND)

    def health[A]: HttpRoute[Any, A] =
      HttpRoute.of({ case GET -> Root / "health" -> _ => UIO(Response.Data(Status.OK)) })

    def app                          = health[ByteBuf] >>> contentLength

  }
}

// - UNSOLVED ISSUES
