package mqtt

import zio._
import zio.stm._

object Mqtt_4 {
  type Topic
  type Data

  trait PahoClient {
    def subscribe(topic: Topic): Unit
    def unsubscribe(topic: Topic): Unit
    def publish(topic: Topic, data: Data): Unit
  }

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

  sealed trait Mqtt[-S, +A] { self =>
    def map[A1](ab: A => A1): Mqtt[S, A1]                          = self.flatMap(m => Mqtt.succeed(ab(m)))
    def flatMap[S1 <: S, A1](ab: A => Mqtt[S1, A1]): Mqtt[S1, A1]  = Mqtt.FFlatmap(self, ab)
    def >>=[S1 <: S, A1 >: A](ab: A => Mqtt[S1, A1]): Mqtt[S1, A1] = self.flatMap(ab)
    def *>[S1 <: S, A1 >: A](ab: Mqtt[S1, A1]): Mqtt[S1, A1]       = self.flatMap(_ => ab)
    def as[S1 <: S, A1 >: A](a1: A1): Mqtt[S1, A1]                 = self.map(_ => a1)
    def nothing: Mqtt[Nothing, Nothing]                            = Mqtt.nothing
    def unit: Mqtt[S, Unit]                                        = self.map(_ => ())    
  }

  object Mqtt {
    case object Empty                                                        extends Mqtt[Any, Nothing]
    case class Read[S]()                                                     extends Mqtt[S, S] {
      def get[S1 >: S, S2 <: S](ref: Ref[S2]): UIO[S1] = ref.get
    }
    case class Succeed[A](message: A)                                        extends Mqtt[Any, A]
    case class Subscribe(topic: Topic)                                       extends Mqtt[Any, Unit]
    case class Unsubscribe(topic: Topic)                                     extends Mqtt[Any, Unit]
    case class Publish(topic: Topic, data: Data)                             extends Mqtt[Any, Unit]
    case class Update[S](ss: S => S)                                         extends Mqtt[S, S]
    case class FFlatmap[S, A0, A1](mqtt: Mqtt[S, A0], ab: A0 => Mqtt[S, A1]) extends Mqtt[S, A1]

    def read[S](): Mqtt[S, S]                                   = Mqtt.Read()
    def update[S](ss: S => S): Mqtt[S, S]                       = Mqtt.Update(ss)
    def publish(topic: Topic, data: Data): Mqtt[Any, Unit]      = Mqtt.Publish(topic, data)
    def succeed[A](message: A): Mqtt[Any, A]                    = Mqtt.Succeed(message)
    def subscribe(topic: Topic): Mqtt[Any, Unit]                = Mqtt.Subscribe(topic)
    def unsubscribe(topic: Topic): Mqtt[Any, Unit]              = Mqtt.Unsubscribe(topic)
    def nothing: Mqtt[Any, Nothing]                             = Mqtt.Empty
    def foreach[S, A](list: List[Mqtt[S, A]]): Mqtt[S, List[A]] = ???

    trait Interpreter {
      def execute[S, A](ref: Ref[S], mqtt: Mqtt[S, A]): IO[Unit, A]
    }

    final case class PahoInterpreter[S](client: PahoClient) extends Interpreter {
      def execute[S, A](ref: Ref[S], mqtt: Mqtt[S, A]): IO[Unit, A] =
        mqtt match {
          // case Mqtt.Update(f: (S => S))  => ref.updateAndGet(f)
          // case m @ Mqtt.Read()           => m.get(ref)
          case Mqtt.Empty                => ZIO.fail(())
          case Mqtt.Succeed(message)     => ZIO.succeed(message)
          case Mqtt.Subscribe(topic)     => UIO(client.subscribe(topic))
          case Mqtt.Unsubscribe(topic)   => UIO(client.unsubscribe(topic))
          case Mqtt.Publish(topic, data) => UIO(client.publish(topic, data))
          case Mqtt.FFlatmap(mqtt, ab)   => execute(ref, mqtt).flatMap(a => execute(ref, ab(a)))
        }
    }
  }

  object Example {
    import Event._

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

    def program(ev: Event): Mqtt[State, Unit] = ev match {
      case User.Subscribed(topic)             => Mqtt.update[State](_.add(topic)).unit
      case User.Unubscribed(topic)            => Mqtt.update[State](_.remove(topic)).unit
      case User.Published(topic, data)        => Mqtt.publish(topic, data)
      // case Broker.ConnectionComplete(reconnect, serverURI) =>
      //   for {
      //     s <- Mqtt.read >>= { s => Mqtt.foreach(s.topics.keys.toList.map(Mqtt.subscribe(_))).unit }

      //   } yield ()
      case Broker.ConnectionLost(cause)       => ???
      case Broker.MessageArrived(topic, data) => ???
      case Broker.DeliveryComplete(message)   => ???
    }

    // val program = Mqtt.of[State] {
    //   case s -> User.Subscribed(topic) => s.add(topic)

    //   case s -> User.Unubscribed(topic) => s.remove(topic)

    //   case _ -> User.Published(topic, data) =>
    //     Mqtt.publish(topic, data).nothing

    //   case s -> User.Subscribed(topic) =>
    //     if (s.count(topic) == 1) Mqtt.subscribe(topic).nothing else Mqtt.nothing

    //   case s -> User.Unubscribed(topic) =>
    //     if (s.count(topic) == 0) Mqtt.unsubscribe(topic).nothing else Mqtt.nothing

    //   case s -> Broker.ConnectionComplete(true, _) =>
    //     Mqtt.subscribe(s.topics.keys.toList).nothing

    //   case _ -> Broker.MessageArrived(topic, data) =>
    //     Mqtt.succeed(process(topic, data)) *> Mqtt.succeed(process(topic, data))
    // }
  }
}
