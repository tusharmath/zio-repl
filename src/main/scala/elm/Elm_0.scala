package mqtt

object Elm_0 {

  sealed trait ELM[-S, -A, +B] { self =>
    def ::[S1 <: S, A1 <: A, B1 >: B](other: ELM[S1, A1, B1]): ELM[S1, A1, B1] = ELM.Concat(self, other)
  }
  object ELM                   {
    final case object Empty                                                   extends ELM[Any, Any, Nothing]
    final case class Init[S](S: S)                                            extends ELM[Any, S, Nothing]
    final case class Total[S, A, B](f: (S, A) => B)                           extends ELM[S, A, B]
    final case class Concat[S, A, B](self: ELM[S, A, B], other: ELM[S, A, B]) extends ELM[S, A, B]

    def update[S, A](f: PartialFunction[(S, A), S]): ELM[S, A, S] =
      ELM.Total((s, a) => if (f.isDefinedAt((s, a))) f(s -> a) else s)

    case class Runnable[F[_], S, A, B](
      init: Init[S],
      update: ELM[S, A, S] = Empty,
      command: ELM[S, A, F[B]] = Empty,
    ) { self =>
      def ++(other: Runnable[F, S, A, B]): Runnable[F, S, A, B] = ???
    }

  }
}
