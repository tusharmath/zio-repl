sealed trait Message[+A, +B] extends Product with Serializable
object Message {
  final case class OperationId(id: String) extends AnyVal
  sealed trait Client[+Q]                  extends Message[Q, Nothing]
  object Client {
    sealed trait Connection extends Client[Nothing]
    object Connection {
      case object Initialize extends Connection
      case object Terminate  extends Connection
    }

    final case class Operation[Q](id: OperationId, signal: Signal[Q]) extends Client[Q]
    sealed trait Signal[Q]
    object Signal {
      final case class Start[Q](query: Q) extends Signal[Q]
      case object Stop                    extends Signal[Nothing]
    }
  }

  sealed trait Server[+D] extends Message[Nothing, D]
  object Server {
    final case class ErrorMessage(message: String, cause: Option[Throwable] = None)

    sealed trait Connection extends Server[Nothing]
    object Connection {
      case object Acknowledge                       extends Connection
      final case class Error(message: ErrorMessage) extends Connection
      case object Terminate                         extends Connection
    }

    final case class Operation[D](id: OperationId, data: Data[D]) extends Server[D]

    sealed trait Data[+D] extends Product with Serializable
    object Data {
      final case class Success[D](data: D, errors: List[ErrorMessage] = Nil) extends Data[D]
      final case class Failure(message: ErrorMessage)                        extends Data[Nothing]
      case object Complete                                                   extends Data[Nothing]
    }
  }
}
