import zio.stream._
import zio._

object GraphQLOperation_0 {
  object GraphQLExecutor {
    def subscribe[Q, A](query: Q): ZStream[Any, Throwable, A] = ???
  }

  // Intent is to have a List[GraphQLOperation]
  
  // You can send a stop signal

  case class OperationId(id: String) extends AnyVal

  case class GraphQLOperation[R, E, A](id: OperationId, stream: ZStream[R, E, A], promise: Promise[E, Unit]) {
    def stop = promise.succeed(())
  }

  object GraphQLOperation {
    def make[Q](id: OperationId, query: Q) =
      for {
        promise <- Promise.make[Throwable, Unit]
      } yield GraphQLOperation(id, GraphQLExecutor.subscribe(query).haltWhen(promise), promise)
  }

  /**
   * program () = { event match { case stop(id) => List.filter(_ === id).foreach(_.stop) } }
   */
}
