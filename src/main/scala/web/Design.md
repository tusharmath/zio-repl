- Create services
  - Endpoints or Routes
    - The have a route to match with
    - A pure function A => B
    - Support encoding / decoding of data
    - Middlewares

- Request & Responses can be of single value or chunked.




case class Http(func: Request => UIO[Response])

sealed trait Request
object Request {
  case class Complete() extends Request
  case class Chunked() extends Request
}

sealed trait Response

object Response {
  case class Http(url: Url, headers: Headers, body: Body) extends Response  
  case class Socket(webSocketMiddleware: Middleware)   extends Response

  object Body {
    case class Buffer(body: byteBuf) extends Body
    case class Chunked(body: ZStream[ByteBuf]) extends Body
  }  
}