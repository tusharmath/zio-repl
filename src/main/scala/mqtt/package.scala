import zio._

package object mqtt {
  trait Reject[+B] {
    def reject: B
  }

  trait OrElse[B] {
    def or(self: B, other: B): B
  }

  trait CommandExecutor[A, B] {
    def execute(A: A): IO[Unit, B]
  }

  case class Update[S, -A](cb: ((S, A)) => S) { self =>
    def ++[A1 <: A](other: Update[S, A1]): Update[S, A1] =
      Update(ev => other.cb(self.cb(ev), ev._2))

    def apply(S: S, E: A): S = cb(S -> E)
  }

  object Update {
    def of[S, A]: CtorOF[S, A] = CtorOF(())

    final case class CtorOF[S, A](unit: Unit) extends AnyVal {
      def apply(pf: PartialFunction[(S, A), S]): Update[S, A] =
        Update[S, A](ev => if (pf.isDefinedAt(ev)) pf(ev) else ev._1)
    }
  }

  case class Command[-S, -A, +B](cb: ((S, A)) => B) { self =>
    def <>[S1 <: S, A1 <: A, B1 >: B](other: Command[S1, A1, B1])(implicit orElse: OrElse[B1]): Command[S1, A1, B1] =
      Command(ev => orElse.or(self.cb(ev), other.cb(ev)))

    def apply(S: S, E: A): B = cb(S -> E)
  }

  object Command {
    def of[S, A]: CtorOF[S, A] = CtorOF(())

    final case class CtorOF[S, A](unit: Unit) extends AnyVal {
      def apply[B](pf: PartialFunction[(S, A), B])(implicit r: Reject[B]): Command[S, A, B] =
        Command(ev => if (pf.isDefinedAt(ev)) pf(ev) else r.reject)
    }
  }

  case class App[S, A, B](init: S, update: Update[S, A], command: Command[S, A, B]) { self =>

    def executor[C](cmdExe: CommandExecutor[B, C]): UIO[AppExecutor[S, A, B, C]] =
      Ref.make(init) map { ref => AppExecutor(ref, self, cmdExe) }
  }

  final case class AppExecutor[S, A, B, C](ref: Ref[S], app: App[S, A, B], executor: CommandExecutor[B, C]) {
    def execute(a: A): IO[Unit, C] = {
      for {
        s <- ref.updateAndGet(s => app.update(s, a))
        a <- executor.execute(app.command(s, a))
      } yield a
    }
  }

}
