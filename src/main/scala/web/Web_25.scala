package web

import zio._
import zio.stream._
import zio.clock.Clock
import javax.xml.crypto.Data

object Web_25 {
  type Endpoint = (Method, URL)
  type Route    = (Method, Path)

  type SocketMiddleware

  sealed trait Status
  object Status {
    case object OK                  extends Status
    case object PAGE_NOT_FOUND      extends Status
    case object UNAUTHORIZED        extends Status
    case object SERVICE_UNAVAILABLE extends Status
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
  final case class Request(endpoint: Endpoint, data: Request.Data)
  object Request {
    final case class Data(headers: List[Header], content: Content)
    sealed trait Content extends Product with Serializable
    object Content {
      case object Empty                                              extends Content
      final case class Complete(bytes: ByteBuf)                      extends Content
      final case class Chunked(data: ZStream[Any, Nothing, ByteBuf]) extends Content
    }
  }

  // RESPONSE
  sealed trait Response extends Product with Serializable { self =>
    def withHeader(header: Header): Response =
      self match {
        case Response.Http(status, headers, content) => Response.Http(status, header :: headers, content)
        case Response.Socket(url, socket)            => self
      }
  }
  object Response {
    final case class Http(status: Status, headers: List[Header] = Nil, content: Content = Content.Empty)
        extends Response

    final case class Socket(url: URL, socket: SocketMiddleware) extends Response

    sealed trait Content extends Product with Serializable
    object Content {
      case object Empty                                              extends Content
      final case class Complete(bytes: ByteBuf)                      extends Content
      final case class Chunked(body: ZStream[Any, Nothing, ByteBuf]) extends Content
    }
  }

  def lift[A, B](pFunc: PartialFunction[A, B]): Function1[A, ZIO[Any, Unit, B]] =
    A => if (pFunc.isDefinedAt(A)) UIO(pFunc(A)) else ZIO.fail(())

  case class Http[-R](private val run: Request => ZIO[R, Unit, Response]) extends AnyVal { self =>
    def <>[R1 <: R](other: Http[R1]): Http[R1]      = Http(req => self(req) <> other(req))
    def apply(req: Request): ZIO[R, Unit, Response] = run(req)
  }

  object Http {
    def responseM[R](response: ZIO[R, Unit, Response]): Http[R] = Http(_ => response)
    def response(response: Response): Http[Any]                 = Http(_ => UIO(response))
    def status(status: Status): Http[Any]                       = Http.response(Response.Http(status))
    def of[R](func: Request => ZIO[R, Unit, Response]): Http[R] = Http(func)
  }

  object Example {
    type Database      = Has[Database.Service]
    type Authenticator = Has[Authenticator.Service]
    object Database      { type Service }
    object Authenticator { type Service }

    def isConnected: ZIO[Database, Nothing, Boolean]                            = ???
    def isAuthenticated(request: Request): ZIO[Authenticator, Nothing, Boolean] = ???

    def health =
      Http.responseM(
        isConnected map {
          case true  => Response.Http(Status.OK)
          case false => Response.Http(Status.SERVICE_UNAVAILABLE)
        }
      )

    def notFound = Http.status(Status.PAGE_NOT_FOUND)

    def setContentLength[R](http: Http[R]): Http[R] =
      Http { req =>
        http(req).map {
          case m @ Response.Http(_, _, Response.Content.Complete(body)) =>
            m.copy(headers = Header.ContentLength(body.readableBytes) :: m.headers)
          case m                                                        => m
        }
      }

    def auth[R](http: Http[R]) =
      Http { req =>
        isAuthenticated(req) >>= {
          case true  => http(req)
          case false => UIO(Response.Http(Status.UNAUTHORIZED))
        }
      }

    def setResponseTiming[R](http: Http[R]) =
      Http { req =>
        for {
          start <- clock.nanoTime
          res   <- http(req)
          end   <- clock.nanoTime
        } yield res.withHeader(Header.X("ResponseTime", (end - start).toString()))
      }

    def server = setContentLength(setResponseTiming(auth(health) <> notFound))
  }
}
