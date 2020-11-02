package web

import zio._
import zio.stream._
import izumi.reflect.macrortti.LightTypeTagRef.Boundaries.Empty

object Web_4 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

  sealed trait Status
  object Status {
    case object OK             extends Status
    case object PAGE_NOT_FOUND extends Status
  }

  case class Header(name: String, value: String)

  case class Http(func: Request => IO[Unit, Response]) { self =>
    def <>(other: Http): Http = Http(req => self.func(req).orElse(other.func(req)))    
  }

  object Http {
    def of(pf: PartialFunction[Request, IO[Unit, Response]]) =
      Http(req => if (pf.isDefinedAt(req)) pf.apply(req) else ZIO.fail(()))

    def route(pf: PartialFunction[(Route, Request.Data), IO[Unit, Response]]) =
      Http.of {
        case Request(method -> url, data) =>
          val route = ((method, url.path), data)
          if (pf.isDefinedAt(route)) pf.apply(route) else ZIO.fail(())
      }
  }

  case class Request(endpoint: Endpoint, data: Request.Data)
  object Request {
    case class Data(headers: List[Header], content: Content)
    sealed trait Content
    object Content {
      case class Complete() extends Content
      case class Chunked()  extends Content
    }
  }

  sealed trait Response

  object Response {
    case class Complete(status: Status, headers: List[Header] = Nil, content: Content = Content.Empty) extends Response
    case class Socket(webSocketMiddleware: SocketMiddleware[Any])                                           extends Response

    sealed trait Content
    object Content {
      case object Empty                                        extends Content
      case class Complete(body: ByteBuf)                       extends Content
      case class Chunked(body: ZStream[Any, Nothing, ByteBuf]) extends Content
    }
  }

  case class Server(port: Int, http: Http)

  object Example {
    import Method._
    import Path._
    import Response._

    def contentLength(http: Http): Http =
      Http.of {
        case req =>
          http.func(req) map {
            case m @ Complete(status, headers, Content.Complete(body)) =>
              m.copy(headers = Header("Content-Length", body.readableBytes.toString()) :: headers)
            case m                                                     => m
          }
      }

    def health =
      Http.route {
        case GET -> _ / "health" -> _ => UIO(Response.Complete(Status.OK))
      }
  }
}
