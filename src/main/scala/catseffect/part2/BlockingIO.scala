package catseffect.part2

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt

object BlockingIO extends IOApp.Simple {

  val someSleeps: IO[Unit] = for {
    _ <- IO.sleep(1.second).debug // SEMANTIC BLOCKING = actual thread is not blocked
    _ <- IO.sleep(1.second).debug
  } yield ()

  // really blocking IOs
  val aBlockingIO: IO[Int] = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    42
  }

  val iosOnManyThread: IO[Unit] = for {
    _ <- IO("first").debug
    _ <- IO.cede // a signal to yield over the thread
    _ <- IO("second").debug
    _ <- IO.cede
  } yield ()

  override def run: IO[Unit] = aBlockingIO.void
}
