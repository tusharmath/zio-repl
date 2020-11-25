package mqtt

import zio.UIO
import zio.ZIO
import zio.IO
import zio.Ref
import zio.stm.STM
import zio.stm.TRef
import zio.stm.ZSTM
import mqtt.Mqtt_0.Mqtt

object Mqtt_0 {
  type Topic
  type Data

  trait PahoClient {
    def subscribe(topic: Topic): Unit
    def unsubscribe(topic: Topic): Unit
    def publish(topic: Topic, data: Data): Unit
  }

  sealed trait Mqtt[+A] { self =>
    def map[A1](ab: A => A1): Mqtt[A1]                               = self.flatMap(m => Mqtt.succeed(ab(m)))
    def flatMap[A1](ab: A => Mqtt[A1]): Mqtt[A1]                     = Mqtt.FFlatmap(self, ab)
    def concat[A1](other: Mqtt[A1]): Mqtt[A1]                        = self.flatMap(_ => other)
    def ++[A1](other: Mqtt[A1]): Mqtt[A1]                            = self.concat(other)
    def >>=[A1](ab: A => Mqtt[A1]): Mqtt[A1]                         = self.flatMap(ab)
    def flatten[A1 >: A, A2](implicit ev: A1 <:< Mqtt[A2]): Mqtt[A2] = self.flatMap(identity(_))
    def as[A1](a1: A1): Mqtt[A1]                                     = self.map(_ => a1)
    def zipWith[A1, C](other: Mqtt[A1])(f: (A, A1) => C): Mqtt[C]    = Mqtt.zipWith(self, other)(f)
    def none: Mqtt[Option[Nothing]]                                  = self.as(None)
  }

  object Mqtt {
    case object Empty extends Mqtt[Unit]

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

    def succeed[A](message: A): Mqtt[A]           = Mqtt.Succeed(message)
    def subscribe(topic: Topic*): Mqtt[Unit]      = Mqtt.Subscribe(topic.toList)
    def subscribe(topic: List[Topic]): Mqtt[Unit] = Mqtt.Subscribe(topic)
    def unsubscribe(topic: Topic): Mqtt[Unit]     = Mqtt.Unsubscribe(topic)
    def empty: Mqtt[Unit]                         = Mqtt.Empty
    def none: Mqtt[Option[Nothing]]               = Mqtt.succeed(None)
    def option[A](A: A): Mqtt[Option[A]]          = Mqtt.succeed(Option(A))

    def zipWith[A0, A1, C](self: Mqtt[A0], other: Mqtt[A1])(f: (A0, A1) => C): Mqtt[C] =
      for {
        a  <- self
        a1 <- other
      } yield f(a, a1)

    trait Interpreter {
      def execute[A](mqtt: Mqtt[A]): UIO[A]
    }

    final case class PahoInterpreter(client: PahoClient) extends Interpreter {
      def execute[A](mqtt: Mqtt[A]): UIO[A] =
        mqtt match {
          case Mqtt.Empty                => ZIO.unit
          case Mqtt.Succeed(message)     => ZIO.succeed(message)
          case Mqtt.Subscribe(topic)     => ZIO.foreach(topic) { o => UIO(client.subscribe(o)) }.unit
          case Mqtt.Unsubscribe(topic)   => UIO(client.unsubscribe(topic))
          case Mqtt.Publish(topic, data) => UIO(client.publish(topic, data))
          case Mqtt.FFlatmap(mqtt, ab)   => execute(mqtt).flatMap(a => execute(ab(a)))
        }
    }
  }

  object Example {
    import Mqtt.Event._

    type Processed
    def process(topic: Topic, message: Data): Processed = ???

    case class State(topics: Map[Topic, Int], inbox: Option[Mqtt.Event]) { self =>
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

    trait Program[S, -A, +B] { self =>
      def update(a: A, s: S): S
      def command(a: A, s: S): Mqtt[B]
    }

    final case class ProgramExecutor[S](ref: Ref[S], paho: Mqtt.PahoInterpreter) {
      def execute[A, B](program: Program[S, A, B], a: A): UIO[B] = {
        for {
          s <- ref.updateAndGet(s => program.update(a, s))
          a <- paho.execute(program.command(a, s))
        } yield a
      }
    }

    object Program extends Program[State, Mqtt.Event, Option[Processed]] {
      override def update(a: Mqtt.Event, s: State): State = a match {
        case User.Subscribed(topic)  => s.add(topic)
        case User.Unubscribed(topic) => s.remove(topic)
        case _                       => s
      }

      override def command(a: Mqtt.Event, s: State): Mqtt[Option[Processed]] =
        a match {
          case User.Subscribed(topic)             => if (s.count(topic) == 1) Mqtt.subscribe(topic).none else Mqtt.none
          case User.Unubscribed(topic)            => if (s.count(topic) == 0) Mqtt.unsubscribe(topic).none else Mqtt.none
          case Broker.ConnectionComplete(true, _) => Mqtt.subscribe(s.topics.keys.toList).none
          case Broker.MessageArrived(topic, data) => Mqtt.option(process(topic, data))
          case _                                  => Mqtt.none
        }
    }
  }

}
