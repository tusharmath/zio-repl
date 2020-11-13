// package web

// import zio._
// import zio.stream._
// import zio.clock.Clock

// // Declarative Encoding
// object Web_16 {
//   type Endpoint = (Method, URL)
//   type Route    = (Method, Path)

//   type SocketMiddleware

//   sealed trait Status
//   object Status {
//     case object OK             extends Status
//     case object PAGE_NOT_FOUND extends Status
//     case object UNAUTHORIZED   extends Status
//   }

//   sealed trait Header
//   object Header {
//     final case class ContentLength(len: Int)              extends Header
//     final case class ContentType(value: ContentTypeValue) extends Header
//     final case class X(name: String, value: String)       extends Header
//     sealed trait ContentTypeValue
//     object ContentTypeValue {
//       case object PlainText       extends ContentTypeValue
//       case object ApplicationJSON extends ContentTypeValue
//     }
//   }

//   // REQUEST
//   final case class Request[+A](endpoint: Endpoint, data: Request.Data[A])
//   object Request {
//     final case class Data[+A](headers: List[Header], content: Content[A])
//     sealed trait Content[+A] extends Product with Serializable
//     object Content {
//       case object Empty                                           extends Content[Nothing]
//       final case class Complete[A](data: A)                       extends Content[A]
//       final case class Chunked[A](data: ZStream[Any, Nothing, A]) extends Content[A]
//     }
//   }

//   // RESPONSE
//   sealed trait Response[+A] extends Product with Serializable { self =>
//     def withHeader(header: Header): Response[A] =
//       self match {
//         case Response.Http(status, headers, content) => Response.Http(status, header :: headers, content)
//         case Response.Socket(url, socket)            => self
//       }
//   }
//   object Response {
//     final case class Http[+A](status: Status, headers: List[Header] = Nil, content: Content[A] = Content.Empty)
//         extends Response[A]

//     final case class Socket(url: URL, socket: SocketMiddleware) extends Response[Nothing]

//     sealed trait Content[+A] extends Product with Serializable
//     object Content {
//       case object Empty                                           extends Content[Nothing]
//       final case class Complete[A](body: A)                       extends Content[A]
//       final case class Chunked[A](body: ZStream[Any, Nothing, A]) extends Content[A]
//     }
//   }

//   sealed trait Http[-R, +X] { self =>
//     def <>[R1 <: R, X1 >: X](other: Http[R1, X1]): Http[R1, X1] =
//       Http.OrElse(self, other)

//     def @@[R1 <: R, X1 >: X](app: HttpMiddleware[R1, X1]): Http[R1, X1] =
//       Http.Middleware(self, app)

//     def apply[X1 >: X](request: Request[X1]): ZIO[R, Unit, Response[X]] =
//       self match {
//         case Http.Constant(response)           => UIO(response)
//         case Http.Make(func)                   => func(request)
//         case Http.OrElse(left, right)          => left(request) <> right(request)
//         case Http.Middleware(http, middleware) =>
//           middleware match {
//             case HttpMiddleware.Convert(converter)  => ???
//             case HttpMiddleware.Concat(left, right) => ???
//           }
//       }
//   }

//   object Http {
//     final case class Constant[X] private (response: Response[X]) extends Http[Any, X]

//     final case class Make[R, X] private (func: Request[Any] => ZIO[R, Unit, Response[X]]) extends Http[R, X]

//     final case class OrElse[R, X] private (left: Http[R, X], right: Http[R, X]) extends Http[R, X]

//     final case class Middleware[R, X] private (http: Http[R, X], middleware: HttpMiddleware[R, X]) extends Http[R, X]

//     final case class Of[X](unit: Unit) extends AnyVal {
//       def apply[R](pFunc: PartialFunction[Request[Any], ZIO[R, Unit, Response[X]]]): Http[R, X] =
//         Make({
//           case a if pFunc.isDefinedAt(a) => pFunc(a)
//           case _                         => ZIO.fail(())
//         })
//     }

//     def of[A]: Of[A]                                     = Of(())
//     def response[X](response: Response[X]): Http[Any, X] = Constant(response)
//   }

//   case class HttpMiddleware[-R, +X](apply: Http[R, X] => Http[R, X]) { self =>
//     def ++[R1 <: R, X1 >: X](other: HttpMiddleware[R1, X1]): HttpMiddleware[R1, X1] = ???
      
//   }

//   object HttpMiddleware {
//     case class Convert[R, X] private (converter: Http[Any, X] => Http[R, X]) extends HttpMiddleware[R, X]

//     case class Concat[R, X] private (left: HttpMiddleware[R, X], right: HttpMiddleware[R, X])
//         extends HttpMiddleware[R, X]

//     final case class Of[X](unit: Unit) extends AnyVal {
//       def apply[R](converter: Http[R, X] => Http[R, X]): HttpMiddleware[R, X] = Convert(converter)
//     }

//     final case class ResponseMiddlewareM[X](unit: Unit) extends AnyVal {
//       def apply[R](func: Response[X] => ZIO[R, Unit, Response[X]]): HttpMiddleware[R, X] =
//         HttpMiddleware.of[X]((app: Http[R, X]) => Http.of[X](a => app(a) >>= func))
//     }

//     final case class ResponseMiddleware[X](unit: Unit) extends AnyVal {
//       def apply[R](pFunc: PartialFunction[Response[X], Response[X]]): HttpMiddleware[R, X] =
//         HttpMiddleware.responseM[X](b => if (pFunc.isDefinedAt(b)) UIO(pFunc(b)) else ZIO.fail(()))
//     }

//     def of[X]: Of[X]                         = Of()
//     def responseM[X]: ResponseMiddlewareM[X] = ResponseMiddlewareM[X]()
//     def response[X]: ResponseMiddleware[X]   = ResponseMiddleware[X]()
//   }

//   object Example {
//     def health = Http.response(Response.Http(Status.OK))

//     def notFound = Http.response(Response.Http(Status.PAGE_NOT_FOUND))

//     type Authenticator = Has[Authenticator.Service]
//     object Authenticator {
//       type Service
//     }

//     def isAuthenticated[X](request: Request[X]): ZIO[Authenticator, Nothing, Boolean] = ???

//     def basicAuth[X] =
//       HttpMiddleware.of[X] { http =>
//         Http.of[X](req =>
//           for {
//             auth <- isAuthenticated(req)
//             res  <- if (auth) http(req) else UIO(Response.Http(Status.UNAUTHORIZED))
//           } yield res
//         )
//       }

//     def setContentLength =
//       HttpMiddleware.response[ByteBuf] {
//         case m @ Response.Http(_, _, Response.Content.Complete(body)) =>
//           m.copy(headers = Header.ContentLength(body.readableBytes) :: m.headers)
//       }

//     def setResponseTiming[X] =
//       HttpMiddleware.of[X] { http =>
//         Http.of[X] { req =>
//           for {
//             start <- clock.nanoTime
//             res0  <- http(req)
//             end   <- clock.nanoTime
//           } yield res0.withHeader(Header.X("ResponseTime", (end - start).toString()))
//         }
//       }

//     def server =
//       health @@ basicAuth <> notFound @@ setContentLength @@ setResponseTiming
//   }
// }

// // Knowns Issues
// // - Middleware is Invariant
