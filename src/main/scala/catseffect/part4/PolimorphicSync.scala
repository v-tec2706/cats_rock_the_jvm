package catseffect.part4

import cats.effect.kernel.MonadCancel
import cats.effect.{IO, IOApp, Sync}

import java.io.{BufferedReader, InputStreamReader}

object PolimorphicSync extends IOApp.Simple {

  // synchronous computation
  trait MySync[F[_]] extends MonadCancel[F, Throwable] {
    def delay[A](thunk: => A): F[A] // "suspension" of a computation - will run on the CE thread pool
    def blocking[A](thunk: => A): F[A] // runs on the blocking thread pool
  }

  // Task 1: Define polymorphic console implementation
  trait Console[F[_]] {
    def println[A](a: A): F[Unit]

    def readLine(): F[String]
  }

  import cats.syntax.functor._

  object Console {
    def apply[F[_]](implicit sync: Sync[F]): F[Console[F]] = sync.pure((System.in, System.out)).map {
      case (in, out) => new Console[F] {
        override def println[A](a: A): F[Unit] = sync.blocking(out.println(a))

        override def readLine(): F[String] = {
          val bufferedReader = new BufferedReader(new InputStreamReader(in))
          sync.blocking(bufferedReader.readLine())
        }
      }
    }
  }

  def consoleReader(): IO[Unit] = for {
    console <- Console.apply[IO]
    _ <- console.println("Hi, what's your name?")
    name <- console.readLine()
    _ <- console.println(s"Nice to meet you $name")
  } yield ()

  override def run: IO[Unit] = consoleReader()
}
