import zio._
import zio.duration._
import scala.language.postfixOps

object Main extends App {

  val resource = console
    .putStrLn("Hello!")
    .repeat(Schedule.spaced(200 milli))
    .interruptible
    .fork
    .toManaged(_.interrupt)

  def run(args: List[String]): zio.URIO[ZEnv, ExitCode] =
    resource.use(_ => ZIO.unit.delay(1 second) *> console.putStrLn("Done!")).exitCode
}
