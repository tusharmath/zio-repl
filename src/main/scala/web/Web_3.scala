package web

import zio.{Task, UIO}
import zio.stream.ZStream
import zio.ZIO
import zio.Cause
import zio.IO

object Web_3 {

  type Response
  sealed trait Header
  object Header                                                                             {
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

  type Endpoint = (Method, Path)
  case class Request(endpoint: Endpoint, headers: List[Header], bytes: Option[ByteBuf])

  object Example {
    import URL._
    import Path._
    import Method._

    def foo(endpoint: Endpoint): Unit =
      endpoint match {
        case GET -> _ / "A" / "B" / "C" => ???
        case GET -> path                => ???
        case _                          => ???
      }
  }

}
