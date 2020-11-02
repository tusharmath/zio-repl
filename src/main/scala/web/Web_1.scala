package web

import zio.{Task, UIO}
import zio.stream.ZStream
import zio.ZIO
import zio.Cause
import zio.IO

object Web_1 {

  type Response

  sealed trait Header
  object Header {
    case class ContentLength(value: Int)             extends Header
    case class ContentType(value: ContentTypeValues) extends Header

    sealed trait ContentTypeValues
    object ContentTypeValues {
      object ApplicationJSON extends ContentTypeValues
    }
  }

  case class HttpMiddleware(req: Request => Task[Request], res: Response => Task[Response]) { self =>
    def <>(other: HttpMiddleware): HttpMiddleware =
      HttpMiddleware(
        req => self.req(req) <> other.req(req),
        res => self.res(res) <> other.res(res)
      )
  }

  // case class Server(port: Int, endpoint: List[Endpoint], middleware: HttpMiddleware)

  // sealed trait Endpoint
  // object Endpoint {
  //   case class Http[E](m: Request => IO[E, Response])                                                   extends Endpoint
  //   case class Socket(path: Url.Relative, m: WebSocketFrame => ZStream[Any, Throwable, WebSocketFrame]) extends Endpoint

  //   def notFound                                                    = Http(_ => ???)
  //   def apply(m: PartialFunction[Request, UIO[Response]]): Endpoint =
  //     Http(req => if (m.isDefinedAt(req)) m.apply(req) else ???)
  // }

  case class Endpoint(method: Method, url: URL)

  case class Request(endpoint: Endpoint, headers: List[Header], bytes: Option[ByteBuf])
  object Request {}

  // Example

}
