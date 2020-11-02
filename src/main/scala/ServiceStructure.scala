import zio._
import zio.stream._

object ServiceStructure {
  type Foo = Has[Foo.Service]
  type Bar = Has[Bar.Service]
  type Baz = Has[Baz.Service]

  object Foo {
    def foo(): URIO[Foo, Unit] = ZIO.accessM[Foo](_.get.foo())

    trait Service {
      def foo(): UIO[Unit]
    }
  }

  object Bar {
    def foo(): URIO[Bar with Foo, Unit] = ZIO.access[Bar](_.get).flatMap(_.bar())

    trait Service {
      def bar(): URIO[Foo, Unit]
    }
  }

  object Baz {
    def baz(): URIO[Baz with Bar, Unit] = ZIO.access[Baz](_.get).flatMap(_.baz())

    trait Service {
      def baz(): URIO[Bar, Unit]
    }
  }
}
