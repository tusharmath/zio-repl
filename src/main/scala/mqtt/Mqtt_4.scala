package mqtt

import zio._
import zio.stm._
import java.util.concurrent.Executor

object Mqtt_4 {
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
  }

  sealed trait MqttApp[+S] { self =>
    def ::[S1 >: S](other: MqttApp[S1]): MqttApp[S1] = MqttApp.Concat(self, other)
    def init[S1 >: S](S: S1): MqttApp.Runnable[S1]   = MqttApp.init(self, S)
  }
  object MqttApp           {

    case class Init[S](S: S) { self =>
      def ++(other: Init[S]): Init[S] = other
    }

    sealed trait Update[S] extends MqttApp[S] { self =>
      def ++(other: Update[S]): Update[S] = Update.concat(self, other)
    }
    object Update {
      final case class Empty[S]()                        extends Update[S]
      final case class Total[S](f: (S, Mqtt.Event) => S) extends Update[S]

      def concat[S](self: Update[S], other: Update[S]): Update[S] = (self, other) match {
        case (Empty(), update)      => update
        case (update, Empty())      => update
        case (Total(f1), Total(f2)) => Total[S]((S, E) => f2(f1(S, E), E))
      }
    }

    sealed trait Command[S] extends MqttApp[S] { self =>
      def ++(other: Command[S]): Command[S] = Command.concat(self, other)
    }
    object Command {
      case class Empty[S]()                                 extends Command[S]
      case class Total[S](f: (S, Mqtt.Event) => Mqtt[Unit]) extends Command[S]

      def concat[S](c1: Command[S], c2: Command[S]): Command[S] = (c1, c2) match {
        case (Empty(), Empty())     => Empty()
        case (c, Empty())           => c
        case (Empty(), c)           => c
        case (Total(f1), Total(f2)) => Total((S, E) => f1(S, E) *> f2(S, E))
      }
    }

    final case class Concat[S](self: MqttApp[S], other: MqttApp[S]) extends MqttApp[S]

    def update[S](pf: PartialFunction[(S, Mqtt.Event), S]): MqttApp[S]           =
      Update.Total[S]((S, E) => if (pf.isDefinedAt(S -> E)) pf(S -> E) else S)

    def command[S](pf: PartialFunction[(S, Mqtt.Event), Mqtt[Unit]]): MqttApp[S] =
      Command.Total[S]((S, E) => if (pf.isDefinedAt(S -> E)) pf(S -> E) else Mqtt.nothing)

    def concat[S](self: MqttApp[S], other: MqttApp[S]): MqttApp[S]               =
      Concat(self, other)

    case class Runnable[S](
      init: Init[S],
      update: Update[S] = Update.Empty[S],
      command: Command[S] = Command.Empty[S],
    ) { self =>
      def ++(other: Runnable[S]): Runnable[S] = Runnable(
        self.init ++ other.init,
        self.update ++ other.update,
        self.command ++ other.command,
      )
    }

    def init[S](app: MqttApp[S], S: S): MqttApp.Runnable[S] = app match {
      case m: MqttApp.Update[S]        => Runnable(init = Init(S), update = m)
      case m: MqttApp.Command[S]       => Runnable(init = Init(S), command = m)
      case MqttApp.Concat(self, other) => self.init(S) ++ other.init(S)
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

    val update = MqttApp.update[State] {
      case s -> User.Subscribed(topic)  => s.add(topic)
      case s -> User.Unubscribed(topic) => s.remove(topic)
    }

    val userCommand = MqttApp.command[State] {
      case _ -> User.Published(topic, data) =>
        Mqtt.publish(topic, data).nothing

      case s -> User.Subscribed(topic) =>
        if (s.count(topic) == 1) Mqtt.subscribe(topic).nothing else Mqtt.nothing
    }

    val brokerCommand = MqttApp.command[State] {
      case s -> User.Unubscribed(topic) =>
        if (s.count(topic) == 0) Mqtt.unsubscribe(topic).nothing else Mqtt.nothing

      case s -> Broker.ConnectionComplete(true, _) =>
        Mqtt.subscribe(s.topics.keys.toList).nothing

      case _ -> Broker.MessageArrived(topic, data) =>
        Mqtt.succeed(process(topic, data)) *> Mqtt.succeed(process(topic, data))
    }

    val d11 = update :: userCommand :: brokerCommand
  }
}
