package catseffect.part2

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{FiberIO, IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object Fibers extends IOApp.Simple {

  val foo: IO[Int] = IO.pure(44)
  val bar: IO[String] = IO.pure("Scala")

  def sameThreadIO(): IO[Unit] = for {
    _ <- foo
    _ <- bar
  } yield ()

  val aFiber: IO[FiberIO[Int]] = foo.debug.start

  def differentThreadIOs(): IO[Unit] = for {
    _ <- aFiber
    _ <- bar.debug
  } yield ()

  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join
  } yield result

  val someIOOnAnotherThread: IO[Outcome[IO, Throwable, Int]] = runOnSomeOtherThread(foo)

  val someResultFromAnotherThread: IO[Int] = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO(-1)
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread(): IO[Outcome[IO, Throwable, Int]] = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel(): IO[Outcome[IO, Throwable, String]] = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug
    // onCancel is a "finalizer", allowing you to free up resources
    val taskWithCancellationHandler = task.onCancel(IO("I'm beeing cancelled!").debug.void)

    for {
      fib <- taskWithCancellationHandler.start
      _ <- IO.sleep(500.millis) >> IO("cancelling").debug
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  /*
   1. Run IO on another thread, return success as IO, an error or cancelled in failed IO
   */
  def processResultsFromFiber[A](io: IO[A]): IO[A] = (for {
    fiber <- io.start
    result <- fiber.join
  } yield result).flatMap {
    case Succeeded(effect) => effect
    case Errored(e) => IO.raiseError(e)
    case Canceled() => IO.raiseError(new RuntimeException("Cancelled"))
  }

  /*
   2. Zip two IOs on different threads
   */
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = for {
    fiba <- ioa.start
    fibb <- iob.start
    resulta <- fiba.join
    resultb <- fibb.join
    res <- (resulta, resultb) match {
      case (Succeeded(a), Succeeded(b)) => a.product(b)
      case (Errored(e1), _) => IO.raiseError(e1)
      case (_, Errored(e2)) => IO.raiseError(e2)
      case _ => IO.raiseError(new RuntimeException("Both failed"))
    }
  } yield res

  def testEx2(): IO[(Int, Int)] = {
    val firstIO = IO.sleep(2.seconds) >> IO(1).debug
    val secIO = IO.sleep(3.seconds) >> IO(2).debug
    tupleIOs(firstIO, secIO)
  }

  /*
   3. Add timeouts to an IO
   */
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] =
    (for {
      fib <- io.start
      _ <- IO.sleep(duration) >> fib.cancel
      result <- fib.join
    } yield result).flatMap {
      case Succeeded(ok) => ok
      case Errored(e) => IO.raiseError(e)
      case Canceled() => IO.raiseError(new RuntimeException("Canceled"))
    }

  def testEx3(): IO[String] = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug
    timeout(aComputation, 2.seconds)
  }

  override def run: IO[Unit] = testEx2().debug.void
}
