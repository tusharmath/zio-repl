package web

import zio.{Task, UIO}
import zio.stream.ZStream
import zio.ZIO
import zio.Cause
import zio.IO

object Web_2 {
  type Request
  type Response

  case class HttpMiddleware(req: Request => Task[Request], res: Response => Task[Response]) { self =>
    def <>(other: HttpMiddleware): HttpMiddleware =
      HttpMiddleware(
        req => self.req(req) <> other.req(req),
        res => self.res(res) <> other.res(res)
      )
  }

  case class Server(port: Int, endpoints: Endpoints, middleware: HttpMiddleware)

  sealed trait Endpoints { self =>
    def ::(other: Endpoints) = Endpoints.Zip(self, other)
  }
  object Endpoints       {
    case class Zip(a: Endpoints, b: Endpoints)        extends Endpoints
    case object Empty                                 extends Endpoints
    case class Http[E](m: Request => IO[E, Response]) extends Endpoints
    case class Socket(path: Path, m: WebSocketFrame => ZStream[Any, Throwable, WebSocketFrame])
        extends Endpoints

    def notFound = Http(_ => ???)

    def partial(m: PartialFunction[Request, UIO[Response]]): Endpoints =
      Http(req => if (m.isDefinedAt(req)) m.apply(req) else ???)
  }

}
