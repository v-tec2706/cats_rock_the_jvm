package catseffect.part3

import cats.effect.kernel.Deferred
import cats.effect.{IO, IOApp, Ref}
import cats.implicits.catsSyntaxParallelTraverse1
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt

object CountDownLatch extends IOApp.Simple {

  override def run: IO[Unit] = test

  def simpleTask(latch: MyCountDownLatch, id: Int): IO[Unit] = for {
    _ <- IO(s"[job-$id] Waiting for signal...").debug
    _ <- latch.await
    _ <- IO(s"[job-$id] Released!").debug
  } yield ()

  def test: IO[Unit] = for {
    latch <- MyCountDownLatch.apply(5)
    jobsFib <- (1 to 5).toList.parTraverse(id => simpleTask(latch, id)).start
    _ <- IO.sleep(1.second)
    _ <- IO("1 release").debug
    _ <- latch.release
    _ <- IO.sleep(1.second)
    _ <- IO("2 release").debug
    _ <- latch.release
    _ <- IO.sleep(1.second)
    _ <- IO("3 release").debug
    _ <- latch.release
    _ <- IO.sleep(1.second)
    _ <- IO("4 release").debug
    _ <- latch.release
    _ <- IO.sleep(1.second)
    _ <- IO("5 release").debug
    _ <- latch.release
    _ <- jobsFib.join
  } yield ()

  // Task 1: Implement count down latch with Ref and Deferred
  abstract class MyCountDownLatch {
    def await: IO[Unit]
    def release: IO[Unit]
  }

  object MyCountDownLatch {
    sealed trait LatchState
    case object Done extends LatchState
    case class Live(count: Int, signal: Deferred[IO, Unit]) extends LatchState

    def apply(size: Int): IO[MyCountDownLatch] = for {
      signal <- Deferred[IO, Unit]
      state <- Ref[IO].of[LatchState](Live(size, signal))
    } yield new MyCountDownLatch {
      override def await: IO[Unit] = state.get.flatMap {
        case Done => IO.unit
        case Live(_, signal) => signal.get
      }

      override def release: IO[Unit] = state.modify {
        case Done => Done -> IO.unit
        case Live(1, signal) => Done -> signal.complete(()).void
        case Live(count, signal) => (Live(count - 1, signal) -> IO.unit)
      }.flatten
    }
  }
}
