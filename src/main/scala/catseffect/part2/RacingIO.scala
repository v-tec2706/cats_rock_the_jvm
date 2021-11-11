package catseffect.part2

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{FiberIO, IO, IOApp, OutcomeIO}
import utils.DebugWrapper

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object RacingIO extends IOApp.Simple {

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"starting computation: $value").debug >>
        IO.sleep(duration) >>
        IO(s"computation: $value - done") >>
        IO(value)
      ).onCancel(IO(s"computation cancelled: $value").debug.void)

  def testRace(): IO[String] = {
    val foo = runWithSleep(11, 1.second)
    val bar = runWithSleep("12", 2.seconds)
    val first: IO[Either[Int, String]] = IO.race(foo, bar)
    /*
      - both IOs run on separate fibers
      - the first one to finish will complete the result
      - the loser will be cancelled
     */

    first.flatMap {
      case Left(value) => IO(s"Foo won: $value")
      case Right(value) => IO(s"Bar won: $value")
    }
  }

  def testRacePair() = {
    val foo = runWithSleep(11, 1.second)
    val bar = runWithSleep("12", 2.seconds)
    val raceResult: IO[Either[
      (OutcomeIO[Int], FiberIO[String]),
      (FiberIO[Int], OutcomeIO[String])
    ]] = IO.racePair(foo, bar)

    raceResult.flatMap {
      case Left((outFoo, fibBar)) => fibBar.cancel >> IO("Foo won").debug >> IO(outFoo).debug
      case Right((fibFoo, outBar)) => fibFoo.cancel >> IO("Bar won").debug >> IO(outBar).debug
    }
  }

  // Task 1: implement a timout pattern with `race()`
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    io.race(IO.sleep(duration)).flatMap {
      case Left(value) => IO(value)
      case Right(_) => IO.raiseError(new RuntimeException("timeout of computation"))
    }

  // Task 2: a method to return LOOSING effect from a race (using `racePair()`)
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    ioa.racePair(iob).flatMap {
      case Left((_, slowFib)) => slowFib.join.flatMap {
        case Succeeded(fa) => fa.map(Right(_))
        case _ => IO.raiseError(new RuntimeException("Execution failed"))
      }
      case Right((slowFib, _)) => slowFib.join.flatMap {
        case Succeeded(fa) => fa.map(Left(_))
        case _ => IO.raiseError(new RuntimeException("Execution failed"))
      }
    }

  // Task 3: implement race in term of racePair
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    ioa.racePair(iob).flatMap {
      case Left((outA, slowFib)) => outA match {
        case Succeeded(fa) => slowFib.cancel >> fa.map(Left(_))
        case Errored(e) => slowFib.cancel >> IO.raiseError(new RuntimeException(s"Execution failed: $e"))
        case Canceled() => slowFib.join.flatMap {
          case Succeeded(effectB) => effectB.map(Right(_))
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both IOs cancelled"))
        }
      }

      case Right((slowFib, outB)) => outB match {
        case Succeeded(fa) => slowFib.cancel >> fa.map(Right(_))
        case Errored(e) => slowFib.cancel >> IO.raiseError(new RuntimeException(s"Execution failed: $e"))
        case Canceled() => slowFib.join.flatMap {
          case Succeeded(effectB) => effectB.map(Left(_))
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both IOs cancelled"))
        }
      }
    }

  override def run: IO[Unit] = timeout(IO.sleep(2.seconds) >> IO(42).debug, 1.seconds).debug.void
}