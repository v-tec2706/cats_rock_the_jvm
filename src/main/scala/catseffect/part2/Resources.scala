package catseffect.part2

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}
import utils.DebugWrapper

import java.io.{File, FileReader}
import java.util.Scanner
import scala.annotation.tailrec
import scala.concurrent.duration._

object Resources extends IOApp.Simple {

  // use case: manage a connection lifecycle
  class Connection(url: String) {
    def open(): IO[String] = IO(s"opening connection to $url").debug

    def close(): IO[String] = IO(s"closing connection to $url").debug
  }

  val asyncFetchUrl: IO[Unit] = for {
    fib <- (new Connection("my.connection").open() *> IO.sleep(Int.MaxValue.seconds)).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  def correctAsyncFetchUrl: IO[Unit] = for {
    conn <- IO(new Connection("my.connection.long"))
    _ <- (conn.open() *> IO.sleep(Int.MaxValue.seconds)).onCancel(conn.close().void).start
  } yield ()

  // bracket pattern - prevents leaking resources
  val bracketFetchUrl: IO[Unit] = IO(new Connection("conn"))
    .bracket(_.open() *> IO.sleep(Int.MaxValue.seconds))(_.close().void)

  val bracketProgram: IO[Unit] = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /* Task 1: process file
    - open a scanner
    - read the file line by line, every 100 milis
    - close the scanner
    - on cancelled/error - close the scanner as well
   */

  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))


  def readScanner(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) {
      IO(scanner.nextLine()).debug >>
      IO.sleep(100.millis) >>
      readScanner(scanner)
    }
    else IO.unit


  def bracketReadFile(path: String): IO[Unit] =
    openFileScanner(path).bracket(scanner => readScanner(scanner))(
      scanner => IO(scanner.close()))

  // Resources
  def connectionFromConfig(path: String): IO[Unit] =
    openFileScanner(path).bracket {
      scanner =>
        IO(new Connection(scanner.nextLine())).bracket { conn =>
          conn.open() >> IO.never
        }(conn => conn.close().void)
    }(scanner => IO("closing file").debug >> IO(scanner.close()))
  // ugly nested resources

  val connectionResource: Resource[IO, Connection] =
    Resource.make(IO(new Connection("my.connection")))(conn => conn.close().void)

  val resourceFetchUrl: IO[Unit] = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()


  // resources are equivalent to brackets
  val simpleResource: IO[String] = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using resource: $string").debug
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing the resource: $string").debug.void

  val usingResourceWithBracket: IO[String] = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource: IO[String] = Resource.make(simpleResource)(releaseResource).use(usingResource)

  // Task 2: convert Task 1 to use resource
  def resourceReadFile(path: String): IO[Unit] = Resource.make(openFileScanner(path)) { scanner =>
    IO("closing file").debug >> IO(scanner.close())
  }.use { scanner =>readScanner(scanner) }

  def cancelReadFile(path: String): IO[Unit] = for {
    fib <- resourceReadFile(path).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  // nested resources
  def connFromConfResource(path: String): Resource[IO, Connection] =
    Resource.make(openFileScanner(path))(scanner => IO("closing file").debug >> IO(scanner.close()))
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void))

  val openConnection: IO[Nothing] = connFromConfResource("some/path").use(conn => conn.open() >> IO.never)

  // finalizers to regular IOs
  val ioWithFinalizer: IO[String] = IO("some resource").debug.guarantee(IO("freeing resource").debug.void)
  val ioWithFinalizer_v2: IO[String] = IO("some resource").debug.guaranteeCase {
    case Succeeded(fa) => fa.flatMap(res => IO(s"releasing resource: $res").debug).void
    case Errored(e) => IO("Nothing to release").debug.void
    case Canceled() => IO("Nothing to release").debug.void
  }

  override def run: IO[Unit] = ioWithFinalizer.void
}
