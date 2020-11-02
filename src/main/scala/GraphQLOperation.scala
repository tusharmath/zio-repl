import zio._
import zio.stream._

object Program {
  type Channel[A]
  case class OperationId(id: String) extends AnyVal

  sealed trait GraphQLOperation[R, E, A0, A1] {
    def withExecutor[R2, E2, A2](fn: A0 => ZStream[R, E, A1]) = ???
    def writeTo(channel: Channel[A1])                         = ???
    def haltOn(promise: Promise[E, Unit])                     = ???
  }

  object GraphQLOperation {
    case class Start[A0](query: A0) extends GraphQLOperation[Any, Nothing, A0, A0]
    case class Stop()               extends GraphQLOperation[Any, Nothing, Any, Nothing]
  }

  // GraphQLOperation
  //   .Start("")
  //   .withExecutor()
  //   .writeTo(ctx)
  //   .haltOn(Promise) // ?? Keep a map of Promises, is not a very clean approach.




}
