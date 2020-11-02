import java.nio.ByteBuffer
import zio.UIO
sealed trait WebSocketFrame extends Product with Serializable
object WebSocketFrame {
  type JFrame
  final case class Binary(buffer: ByteBuffer)                 extends WebSocketFrame
  final case class Text(text: String)                         extends WebSocketFrame
  final case class Close(status: Int, reason: Option[String]) extends WebSocketFrame
  final case class Ping()                                     extends WebSocketFrame
  final case class Pong()                                     extends WebSocketFrame
  final case class Continuation(buffer: ByteBuffer)           extends WebSocketFrame

  def fromJFrame(j: JFrame): UIO[WebSocketFrame] = ???
  def toJFrame(frame: WebSocketFrame): UIO[JFrame] = ???
}
