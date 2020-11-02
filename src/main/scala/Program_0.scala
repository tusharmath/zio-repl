import zio.stream._
import zio._
import Message._
import Message.Client._
import Message.Client.Connection.Initialize
import Message.Client.Connection.Terminate
import Message.Client.Signal.Start
import Message.Client.Signal.Stop
import WebSocketFrame.Binary
import WebSocketFrame.Text
import WebSocketFrame.Close
import WebSocketFrame.Ping
import WebSocketFrame.Pong
import WebSocketFrame.Continuation
import Program_0.EventPayload.ExceptionCaught
import Program_0.EventPayload.Read
import Program_0.EventPayload.UserEventTriggered
import Program_0.EventPayload.Active
import Program_0.EventPayload.Inactive
import Program_0.EventPayload.ReadComplete
import Program_0.EventPayload.Registered
import Program_0.EventPayload.Unregistered
import Program_0.EventPayload.WritabilityChanged
import zio.console._
import zio.duration._
import zio.clock.Clock

object Program_0 {

  // Middleware works with some incoming and outgoing messages
  // --- GQL Middleware(GQL messages) --- WebSocket Middleware(WebSocket Messages) --- ChannelMiddleware (Channel Messages) --- ChannelHandler (write or close)

  // Services
  type GraphQLExecutor = Has[GraphQLExecutor.Service]
  object GraphQLExecutor {
    trait Service
    def subscribe[Q, A](query: Q): ZStream[GraphQLExecutor, Throwable, A] = ???
  }

  type ClientDecoder = Has[ClientDecoder.Service]
  type GraphQLRequest
  type GraphQLResponse
  object ClientDecoder {
    trait Service
    def decode(string: String): RIO[ClientDecoder, Client[GraphQLRequest]] = ???
  }

  type ServerEncoder = Has[ServerEncoder.Service]
  object ServerEncoder {
    trait Service
    def encode(message: Server[GraphQLResponse]): RIO[ServerEncoder, String] = ???
  }

  type ChannelHandler = Has[ChannelHandler.Service]
  type JChannelHandlerContext
  object ChannelHandler {
    trait Service
    def writeAndFlush(c: JChannelHandlerContext, a: Any): RIO[ChannelHandler, Unit] = ???
    def close(c: JChannelHandlerContext): RIO[ChannelHandler, Unit]                 = ???
  }

  /// Core Data Structures

  case class ChannelOperationState() {
    def onComplete(id: OperationId): UIO[Unit] = ???
    def complete(id: OperationId): UIO[Unit]   = ???
  }

  // Helps
  def success[A](id: OperationId, data: A) =
    Server.Operation(id, Server.Data.Success(data))

  def failure(id: OperationId, cause: Throwable) =
    Server.Operation(id, Server.Data.Failure(Server.ErrorMessage("GraphQL error", Option(cause))))

  // Middleware
  case class Middleware[-R, +E, -A, +B](feed: A => ZStream[R, E, B]) { self =>
    def feedM[R1 <: R, E1 >: E, A1 <: A](zio: ZIO[R1, E1, A1]) = ZStream.unwrap(zio.map(feed))

    def map[B1](f: B => B1): Middleware[R, E, A, B1] =
      Middleware(event => feed(event).map(f))

    def flatMap[R1 <: R, E1 >: E, A1 <: A, B1](f: B => Middleware[R1, E1, A1, B1]): Middleware[R1, E1, A1, B1] =
      Middleware(a => feed(a).flatMap(b => f(b).feed(a)))

    def mapM[R1 <: R, E1 >: E, B1](f: B => ZIO[R1, E1, B1]): Middleware[R1, E1, A, B1] =
      Middleware(event => feed(event).mapM(f))

    def contramapM[R1 <: R, E1 >: E, A1](f: A1 => ZIO[R1, E1, A]): Middleware[R1, E1, A1, B] =
      Middleware(event => feedM(f(event)))

    def contramap[A1](f: A1 => A): Middleware[R, E, A1, B] =
      Middleware(event => feed(f(event)))

    def zipRight[R1 <: R, E1 >: E, A1 <: A, B1](other: Middleware[R1, E1, A1, B1]): Middleware[R1, E1, A1, B1] =
      self.flatMap(_ => other)

  }

  def gqlMiddleware[Q, D]: Middleware[GraphQLExecutor, Nothing, (ChannelOperationState, Client[Q]), Server[D]] =
    Middleware({
      case s -> message =>
        message match {
          case Operation(id, signal) =>
            signal match {
              case Start(query) =>
                GraphQLExecutor
                  .subscribe(query)
                  .map(data => success(id, data))
                  .haltWhen(s.onComplete(id))
                  .catchAll(cause => ZStream.succeed(failure(id, cause)))
              case Stop         => ZStream.unwrap(s.complete(id).as(ZStream.empty))
            }
          case Initialize            => ZStream.succeed(Server.Connection.Acknowledge)
          case Terminate             => ZStream.empty
        }
    })

  def webSocketMiddleware[R, E >: Throwable](
    m: Middleware[R, E, (ChannelOperationState, Client[GraphQLRequest]), Server[GraphQLResponse]]
  ): Middleware[R with ClientDecoder with ServerEncoder, E, (ChannelOperationState, WebSocketFrame), WebSocketFrame] =
    Middleware({
      case s -> message =>
        message match {
          case Text(text)            =>
            m.feedM(ClientDecoder.decode(text).map(s -> _))
              .mapM(message => ServerEncoder.encode(message))
              .map(WebSocketFrame.Text(_))
          case Ping()                => ZStream.succeed(Pong())
          case Pong()                => ZStream.succeed(Ping())
          case Binary(_)             => ZStream.empty
          case Close(status, reason) => ZStream.empty
          case Continuation(_)       => ZStream.empty
        }
    })

  type ChannelEvent[+A, +B] = (A, EventPayload[B])
  sealed trait EventPayload[+O] extends Product with Serializable
  object EventPayload {
    final case class ExceptionCaught(cause: Throwable) extends EventPayload[Nothing]
    final case class Read[O](message: O)               extends EventPayload[O]
    final case class UserEventTriggered(message: Any)  extends EventPayload[Nothing]
    final case object Active                           extends EventPayload[Nothing]
    final case object Inactive                         extends EventPayload[Nothing]
    final case object ReadComplete                     extends EventPayload[Nothing]
    final case object Registered                       extends EventPayload[Nothing]
    final case object Unregistered                     extends EventPayload[Nothing]
    final case object WritabilityChanged               extends EventPayload[Nothing]
  }

  import WebSocketFrame._

  def channelMiddleware[R, E >: Throwable, A, B, C](
    s: ChannelOperationState,
    m: Middleware[R, E, (ChannelOperationState, WebSocketFrame), WebSocketFrame]
  ): Middleware[Clock with ChannelHandler with Console with R, E, ChannelEvent[
    JChannelHandlerContext,
    JFrame
  ], Nothing] =
    Middleware({
      case context -> event =>
        event match {
          case Read(message)               =>
            ZStream
              .fromEffect(WebSocketFrame.fromJFrame(message))
              .flatMap(m.feed(s, _))
              .mapM(WebSocketFrame.toJFrame(_))
              .mapM(data => ChannelHandler.writeAndFlush(context, data)) *> ZStream.empty
          case ExceptionCaught(cause)      => ZStream.unwrap(console.putStrLn(s"Error: ${cause}").as(ZStream.empty))
          case UserEventTriggered(message) =>
            ZStream.unwrap(ChannelHandler.close(context).delay(300 seconds).as(ZStream.empty))
          case _                           => ZStream.empty
        }
    })
}
