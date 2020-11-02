import zio.stream.ZStream
object GraphQLExecutor {
  def subscribe[Q, A](query: Q): ZStream[Any, Throwable, A] = ???
}
