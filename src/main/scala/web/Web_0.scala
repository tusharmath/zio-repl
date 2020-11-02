package web

import zio.{Task, UIO}

object Web {
  type Request
  type Response

  // - Can work for contentLength or both Requests and Responses
  case class HttpMiddleware(req: Request => Task[Request], res: Response => Task[Response] = UIO(_)) { self =>
    def ++(other: HttpMiddleware): HttpMiddleware =
      HttpMiddleware(
        self.req(_) >>= other.req,
        self.res(_) >>= other.res
      )

    def <>(other: HttpMiddleware): HttpMiddleware =
      HttpMiddleware(
        r => self.req(r) <> other.req(r),
        r => self.res(r) <> other.res(r)
      )
  }

  case class Server(port: Int, endpoints: List[Endpoint] = Nil)

  sealed trait Endpoint
  object Endpoint {
    case class Http(m: HttpMiddleware)                 extends Endpoint
    case class Socket(path: Path, m: SocketMiddleware[Any]) extends Endpoint
  }
}
