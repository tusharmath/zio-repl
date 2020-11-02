import web.Path./
import web.Path.Root
package object web {
  type WebSocketFrame

  type Middleware[-R, +E, -A, +B]

  type SocketMiddleware[R] = Middleware[R, Unit, WebSocketFrame, WebSocketFrame]

  trait ByteBuf {
    def readableBytes: Int
  }

  sealed trait Method
  object Method {
    object GET  extends Method
    object PUT  extends Method
    object POST extends Method
  }

  sealed trait Scheme
  object Protocol {
    case object HTTP  extends Scheme
    case object HTTPS extends Scheme
  }

  sealed trait Path { self =>
    def asString: String
    def /(name: String): Path      = append(name)
    def append(name: String): Path = if (name.isEmpty) this else Path./(this, name)
    def toList: List[String]
    override def toString: String  = this.asString
    def toURL: URL                 = URL(self, URL.Connection.Relative)
  }

  object Path {
    def apply(): Path                               = Root
    def apply(string: String): Path                 = if (string.isBlank) Root else Path(string.split("/").toList)
    def apply(seqString: String*): Path             = Path(seqString.toList)
    def apply(list: List[String]): Path             = list.foldLeft[Path](Root)((a, s) => a.append(s))
    def unapply(path: Path): Option[String]         =
      path match {
        case _ / name => Option(name)
        case Root     => None
      }
    def unapplySeq(arg: Path): Option[List[String]] = Option(arg.toList)
    def empty: Path                                 = Root

    final case class /(path: Path, name: String) extends Path {
      override def asString: String     = s"${path.asString}/${name}"
      override def toList: List[String] = path.toList ::: List(name)
    }

    final case object Root extends Path {
      def asString: String     = ""
      def toList: List[String] = Nil
    }

  }

  case class URL(path: Path, connection: URL.Connection)
  object URL {
    sealed trait Connection
    object Connection {
      case object Relative                                         extends Connection
      case class Absolute(scheme: Scheme, host: String, port: Int) extends Connection
    }
  }
}
