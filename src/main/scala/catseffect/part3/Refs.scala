package catseffect.part3

import cats.effect.{IO, IOApp, Ref}
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt

object Refs extends IOApp.Simple {

  // ref = purely functional atomic reference
  val atomicFoo: IO[Ref[IO, Int]] = Ref[IO].of(42)

  // why: concurrent  + thread-safe reads/writes over shared values, in a purely functional way

  import cats.syntax.parallel._

  def demoConcurrentWorkImpure(): IO[Unit] = {
    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordLength = workload.length
      for {
        _ <- IO(s"Length of word '$workload': $wordLength")
        newCount <- IO(count + wordLength)
        _ <- IO(s"New total: $newCount").debug
        _ <- IO(count += newCount)
      } yield ()
    }

    List("foo", "fooBar", "foo1Bar2")
      .map(task)
      .parSequence
      .void
  }
  /*
  above is:
   - mix of pure/impure
   - not thread safe
   */

  def demoConcurrentWorkPure(): IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val wordLength = workload.length
      for {
        _ <- IO(s"Length of word '$workload': $wordLength")
        newCount <- total.updateAndGet(currentCount => currentCount + wordLength)
        _ <- IO(s"New total: $newCount").debug
      } yield ()
    }

    for {
      initialCount <- Ref[IO].of(0)
      _ <- List("foo", "fooBar", "foo1Bar2")
        .map(string => task(string, initialCount))
        .parSequence
    } yield ()
  }

  // Task 1: refactor
   def tickingClockImpure(): IO[Unit] = {
     var ticks: Long = 0L
     def tickingClock: IO[Unit] = for {
       _ <- IO.sleep(1.second)
       _ <- IO(System.currentTimeMillis()).debug
       _ <- IO(ticks += 1)
       _ <- tickingClock
     } yield ()

     def printTicks: IO[Unit] = for {
       _ <- IO.sleep(5.seconds)
       _ <- IO(s"TICKS: $ticks").debug
       _ <- printTicks
     } yield ()

     for {
       _ <- (tickingClock, printTicks).parTupled
     } yield ()
   }

  def tickingClockPure(): IO[Unit] = {
    val ticks: IO[Ref[IO, Long]] = Ref[IO].of(0L)

    def tickingClock(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      tickRes <- ticks.get
      _ <- IO(s"TICKS: ${tickRes}").debug
      _ <- printTicks(ticks)
    } yield ()

    for {
      t <- ticks
      _ <- (tickingClock(t), printTicks(t)).parTupled
    } yield ()
  }



  override def run: IO[Unit] = tickingClockPure()


}
