import zio.stream.ZStream
import ApplicationArchitecture.GraphQLOperation.Start
import ApplicationArchitecture.GraphQLOperation.Stop
import zio._

object ApplicationArchitecture {
  case class OperationId(id: String) extends AnyVal
  object Message {
    sealed trait Client[+Q] extends Product with Serializable
    object Client {
      sealed trait Connection extends Client[Nothing]
      object Connection {
        case object Initialize extends Connection
        case object Terminate  extends Connection
      }

      case class Operation[Q](id: OperationId, signal: Signal[Q]) extends Client[Q]
      sealed trait Signal[Q]
      object Signal {
        case class Start[Q](query: Q) extends Signal[Q]
        case object Stop              extends Signal[Nothing]
      }
    }

    sealed trait Server[+D] extends Product with Serializable
    object Server {
      case class ErrorMessage(message: String, cause: Option[Throwable] = None)

      sealed trait Connection extends Server[Nothing]
      object Connection {
        case object Acknowledge                 extends Connection
        case class Error(message: ErrorMessage) extends Connection
        case object Terminate                   extends Connection
      }

      case class Operation[D](id: OperationId, message: Message[D]) extends Server[D]

      sealed trait Message[D]
      object Message {
        case class Data[D](data: D, errors: List[ErrorMessage]) extends Message[D]
        case class Error(message: ErrorMessage)                 extends Message[Nothing]
        case object Complete                                    extends Message[Nothing]
      }
    }
  }

  sealed trait GraphQLOperation[+Q] extends Serializable with Product
  object GraphQLOperation {
    case class Start[Q](query: Q) extends GraphQLOperation[Q]
    case object Stop              extends GraphQLOperation[Nothing]
  }

  trait Channel[A] {
    def writeAndFlush(data: A): Unit
    def close(): Unit
  }

  object GraphQLExecutor {
    def subscribe[Q, A](query: Q): ZStream[Any, Nothing, A] = ???
  }

  def interpret[Q, A](operation: GraphQLOperation[Q], context: Channel[A], promise: Promise[Throwable, Unit]) =
    operation match {
      case Start(query) =>
        GraphQLExecutor.subscribe(query).mapM(data => Task(context.writeAndFlush(data))).haltWhen(promise)
      case Stop         => promise.succeed(())
    }

}
