package web

import zio._
import zio.stream._
import zio.clock.Clock
import javax.xml.crypto.Data

object Web_23 {
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

  final case class Http[-R, +S] private[Http] (run: Request => ZIO[R, Unit, (Response, S)]) { self =>
    def <>[R1 <: R, S1 >: S](other: Http[R1, S1]): Http[R1, S1] =
      Http(req => self.run(req) <> other.run(req))

    def modifyM[R1 <: R](func: Response => ZIO[R1, Unit, Response]): Http[R1, S] =
      Http(req => run(req) >>= { case (res, s) => func(res).map((_, s)) })

    def modify(func: PartialFunction[Response, Response]): Http[R, S]            =
      modifyM(lift(func))

    def map[S1](ss: S => S1): Http[R, S1]                     =
      Http(req => self.run(req).map({ case (res, s) => (res, ss(s)) }))

    def >>=[R1 <: R, S1](ss: S => Http[R1, S1]): Http[R1, S1] =
      self.flatMap(ss)

    def flatMap[R1 <: R, S1](ss: S => Http[R1, S1]): Http[R1, S1] =
      Http(req => self.run(req) >>= { case (res, s) => ss(s).run(req) })
  }

  object Http {
    def statusM[R](s: ZIO[R, Unit, Status]): Http[R, Unit] =
      Http(_ => s.map(code => (Response.Http(code), ())))

    def status(code: Status): Http[Any, Unit] =
      Http(_ => UIO(Response.Http(code), ()))

    def response(res: Response): Http[Any, Unit] =
      Http(_ => UIO((res, ())))

    def responseM[R](resM: ZIO[R, Unit, Response]): Http[R, Unit] =
      Http(_ => resM.map(res => (res, ())))

    def empty: Http[Any, Unit] = Http(_ => ZIO.fail(()))

    def serviceM[R, S](cb: Request => ZIO[R, Unit, S]): Http[R, S] =
      ???

    def effect[R, S](cb: ZIO[R, Unit, S]): Http[R, S] =
      ???

    def success[S](s: S): Http[Any, S] =
      ???
  }

  object Example {
    object Database      { type Service }
    object Authenticator { type Service }

    type Database      = Has[Database.Service]
    type Authenticator = Has[Authenticator.Service]

    // Services
    def isAuthenticated(request: Request): ZIO[Authenticator, Nothing, Boolean] = ???
    def isConnected: ZIO[Database, Nothing, Boolean]                            = ???

    // Routes
    def health =
      Http.statusM {
        isConnected map {
          case true  => Status.OK
          case false => Status.SERVICE_UNAVAILABLE
        }
      }

    def notFound = Http.status(Status.PAGE_NOT_FOUND)

    // Middleware
    def setContentLength[R, S](http: Http[R, S]) =
      http.modify {
        case m @ Response.Http(_, _, Response.Content.Complete(body)) =>
          m.copy(headers = Header.ContentLength(body.readableBytes) :: m.headers)
      }

    def auth[R, S](http: Http[R, S]) =
      for {
        cond <- Http.serviceM(isAuthenticated)
        app = cond match {
          case true  => http
          case false => Http.status(Status.UNAUTHORIZED)
        }
      } yield app

    def setResponseTiming[R, S](http: Http[R, S]) =
      for {
        start <- Http.effect(clock.nanoTime)
        s     <- http
        end   <- Http.effect(clock.nanoTime)
      } yield s

    // def setResponseTiming =
    //   HttpMiddleware.of { (req, http) =>
    //     for {
    //       start <- clock.nanoTime
    //       res   <- Http.execute(http, req)
    //       end   <- clock.nanoTime
    //     } yield res.withHeader(Header.X("ResponseTime", (end - start).toString()))
    //   }

    // def server = health @@ Authenticator.auth <> notFound @@ setResponseTiming @@ setContentLength
  }
}

// - UNSOLVED ISSUES
