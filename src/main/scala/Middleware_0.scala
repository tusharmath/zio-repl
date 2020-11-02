import zio.stream.ZStream
import zio.ZIO
object Middleware_0 {
  sealed trait Middleware[-R, +E, -A, +B] { self =>
    def and[R1 <: R, E1 >: E, C](that: Middleware[R1, E1, B, C]): Middleware[R1, E1, A, C] =
      Middleware.And(self, that)

    def or[R1 <: R, E1 >: E, A1 <: A, B1 >: B](that: Middleware[R1, E1, A1, B1]): Middleware[R1, E1, A1, B1] =
      Middleware.Or(self, that)

    def mapM[R1 <: R, E1 >: E, C](bc: B => ZIO[R1, E1, C]): Middleware[R1, E1, A, C] = ???

    def collect(A: A): ZStream[R, E, B] = Middleware.collect(self)(A)
  }

  object Middleware {
    final case class And[R, E, A, B, C](first: Middleware[R, E, A, B], second: Middleware[R, E, B, C])
        extends Middleware[R, E, A, C]

    final case class Or[R, E, A, B](first: Middleware[R, E, A, B], second: Middleware[R, E, A, B])
        extends Middleware[R, E, A, B]

    def collect[R, E, A, B](m: Middleware[R, E, A, B])(A: A): ZStream[R, E, B] =
      m match {
        case And(first, second) => first.collect(A).flatMap(B => second.collect(B))
        case Or(first, second)  => first.collect(A) *> second.collect(A)
      }
  }
}
