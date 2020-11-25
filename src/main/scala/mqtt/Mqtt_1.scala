package mqtt

import zio._
import zio.stm._

object Mqtt_1 {
  type Topic
  type Data

  trait PahoClient {
    def subscribe(topic: Topic): Unit
    def unsubscribe(topic: Topic): Unit
    def publish(topic: Topic, data: Data): Unit
  }

  sealed trait Mqtt[+A] { self =>
    def map[A1](ab: A => A1): Mqtt[A1]           = self.flatMap(m => Mqtt.succeed(ab(m)))
    def flatMap[A1](ab: A => Mqtt[A1]): Mqtt[A1] = Mqtt.FFlatmap(self, ab)
    def >>=[A1](ab: A => Mqtt[A1]): Mqtt[A1]     = self.flatMap(ab)
    def *>[A1](ab: Mqtt[A1]): Mqtt[A1]           = self.flatMap(_ => ab)
    def as[A1](a1: A1): Mqtt[A1]                 = self.map(_ => a1)
    def nothing: Mqtt[Nothing]                   = Mqtt.nothing
  }

  object Mqtt {
    case object Empty extends Mqtt[Nothing]

    case class Succeed[A](message: A)            extends Mqtt[A]
    case class Subscribe(topic: List[Topic])     extends Mqtt[Unit]
    case class Unsubscribe(topic: Topic)         extends Mqtt[Unit]
    case class Publish(topic: Topic, data: Data) extends Mqtt[Unit]

    case class FFlatmap[A0, A1](mqtt: Mqtt[A0], ab: A0 => Mqtt[A1]) extends Mqtt[A1]

    sealed trait Event
    object Event {
      sealed trait User extends Event
      object User {
        case class Subscribed(topic: Topic)            extends User
        case class Unubscribed(topic: Topic)           extends User
        case class Published(topic: Topic, data: Data) extends User
      }

      sealed trait Broker extends Event
      object Broker {
        case class ConnectionComplete(reconnect: Boolean, serverURI: String) extends Broker
        case class ConnectionLost(cause: Throwable)                          extends Broker
        case class MessageArrived(topic: Topic, data: Data)                  extends Broker
        case class DeliveryComplete(message: Data)                           extends Broker
      }
    }

    def publish(topic: Topic, data: Data): Mqtt[Unit] = Mqtt.Publish(topic, data)
    def succeed[A](message: A): Mqtt[A]               = Mqtt.Succeed(message)
    def subscribe(topic: Topic*): Mqtt[Unit]          = Mqtt.Subscribe(topic.toList)
    def subscribe(topic: List[Topic]): Mqtt[Unit]     = Mqtt.Subscribe(topic)
    def unsubscribe(topic: Topic): Mqtt[Unit]         = Mqtt.Unsubscribe(topic)
    def nothing: Mqtt[Nothing]                        = Mqtt.Empty
    def update[S]: Update.CtorOF[S, Mqtt.Event]       = Update.CtorOF(())
    def command[S]: Command.CtorOF[S, Mqtt.Event]     = Command.CtorOF(())

    trait Interpreter {
      def execute[A](mqtt: Mqtt[A]): IO[Unit, A]
    }

    final case class PahoInterpreter(client: PahoClient) extends Interpreter {
      def execute[A](mqtt: Mqtt[A]): IO[Unit, A] =
        mqtt match {
          case Mqtt.Empty                => ZIO.fail(())
          case Mqtt.Succeed(message)     => ZIO.succeed(message)
          case Mqtt.Subscribe(topic)     => ZIO.foreach(topic) { o => UIO(client.subscribe(o)) }.unit
          case Mqtt.Unsubscribe(topic)   => UIO(client.unsubscribe(topic))
          case Mqtt.Publish(topic, data) => UIO(client.publish(topic, data))
          case Mqtt.FFlatmap(mqtt, ab)   => execute(mqtt).flatMap(a => execute(ab(a)))
        }
    }

    trait Reject[+B] {
      def reject: B
    }

    case class Update[S, -A](cb: ((S, A)) => S) {
      def apply(S: S, E: A): S = cb(S -> E)
    }
    object Update                               {
      def of[S, A]: CtorOF[S, A] = CtorOF(())

      final case class CtorOF[S, A](unit: Unit) extends AnyVal {
        def apply(pf: PartialFunction[(S, A), S]): Update[S, A] =
          Update[S, A](ev => if (pf.isDefinedAt(ev)) pf(ev) else ev._1)
      }
    }

    case class Command[-S, -A, +B](cb: ((S, A)) => B) {
      def apply(S: S, E: A): B = cb(S -> E)
    }

    object Command {
      def of[S, A]: CtorOF[S, A] = CtorOF(())

      final case class CtorOF[S, A](unit: Unit) extends AnyVal {
        def apply[B](pf: PartialFunction[(S, A), B])(implicit r: Reject[B]): Command[S, A, B] =
          Command(ev => if (pf.isDefinedAt(ev)) pf(ev) else r.reject)
      }
    }

    case class App[S, A](
      init: S,
      update: Update[S, Mqtt.Event],
      command: Command[S, Mqtt.Event, Mqtt[A]],
    )

    final case class ProgramExecutor[S](ref: Ref[S], paho: Mqtt.PahoInterpreter) {
      def execute[A](program: App[S, A], a: Mqtt.Event): IO[Unit, A] = {
        for {
          s <- ref.updateAndGet(s => program.update(s, a))
          a <- paho.execute(program.command(s, a))
        } yield a
      }
    }
  }

  object Example {
    import Mqtt.Event._

    type Processed
    def process(topic: Topic, message: Data): Processed = ???

    case class State(topics: Map[Topic, Int]) { self =>
      def add(topic: Topic): State =
        self.copy(topics = topics.updatedWith(topic) {
          case None      => Option(1)
          case Some(int) => Option(int + 1)
        })

      def remove(topic: Topic): State =
        self.copy(topics = topics.updatedWith(topic) {
          case None      => Option(0)
          case Some(int) => Option(int + 1)
        })

      def count(topic: Topic): Int = ???

      def countIs(topic: Topic, i: Int): Boolean = ???
    }

    implicit def mqttFailure[B]: Mqtt.Reject[Mqtt[B]] = new Mqtt.Reject[Mqtt[B]] {
      def reject: Mqtt[B] = Mqtt.nothing
    }

    val d11App = Mqtt.App[State, Processed](
      init = State(Map.empty),
      update = Mqtt.update {
        case s -> User.Subscribed(topic)  => s.add(topic)
        case s -> User.Unubscribed(topic) => s.remove(topic)
      },
      command = Mqtt.command {
        case _ -> User.Published(topic, data) =>
          Mqtt.publish(topic, data).nothing

        case s -> User.Subscribed(topic) =>
          if (s.count(topic) == 1) Mqtt.subscribe(topic).nothing else Mqtt.nothing

        case s -> User.Unubscribed(topic) =>
          if (s.count(topic) == 0) Mqtt.unsubscribe(topic).nothing else Mqtt.nothing

        case s -> Broker.ConnectionComplete(true, _) =>
          Mqtt.subscribe(s.topics.keys.toList).nothing

        case _ -> Broker.MessageArrived(topic, data) =>
          Mqtt.succeed(process(topic, data)) *> Mqtt.succeed(process(topic, data))
      },
    )
  }
}
