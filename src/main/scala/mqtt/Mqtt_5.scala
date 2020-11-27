package mqtt

import zio._
import zio.stm._

object Sum_5 {
  sealed trait Sum[-S, +A] {}

  object Sum {
    case class Foo[S](ab: S => S) extends Sum[S, S] {}

    def execute[S, A](mqtt: Sum[S, A], s: S): A =
      mqtt match {
        case m @ Sum.Foo(ss) => ss(s)
      }
  }
}
