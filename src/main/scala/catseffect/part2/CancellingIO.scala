package catseffect.part2

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt

object CancellingIO extends IOApp.Simple {

  val chainOfIOs: IO[Int] = IO("waiting").debug >> IO.canceled >> IO(42).debug

  val specialPaymentSystem: IO[String] = (
    IO("Payment running, don't cancel me...").debug >>
      IO.sleep(1.second) >>
      IO("Payment completed").debug
    ).onCancel(IO("CANCELLED").debug.void)

  val cancellation: IO[Unit] = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicOperation: IO[String] = IO.uncancelable(_ => specialPaymentSystem) // "masking"

  val noCancellation: IO[Unit] = for {
    fib <- atomicOperation.start
    _ <- IO.sleep(500.millis) >> IO("attempting cancellation...").debug >> fib.cancel
    _ <- fib.join
  } yield ()

  /*
  The Pool object can be used to mark sections within the returned effect which CAN BE CANCELLED

  Example: authentication service with two parts:
   - input password, can be cancelled, because otherwise we might block indefinitely on user input
   - verify password - CANNOT be cancelled once it's started
   */

  val inputPassword: IO[String] = IO("Input password: ").debug >> IO("(typing password)").debug >> IO.sleep(2.seconds) >> IO("password123!!")
  val verifyPassword: String => IO[Boolean] = (pw: String) => IO("verifying...").debug >> IO.sleep(2.seconds) >> IO(true)

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out").debug.void) // is cancellable
      verified <- verifyPassword(pw)
      _ <- if (verified) IO("Authentication successful").debug
      else IO("Authentication failed").debug
    } yield ()
  }

  val authProgram: IO[Unit] = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3.seconds) >> IO("Authenticating timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  val program: IO[Unit] = for {
    _ <- IO("Started").debug
    fib <- (IO("Switching thread -- ").debug >> IO.sleep(2.second) >> IO("still on another thread").debug).start
    _ <- IO("... on main").debug
    _ <- fib.join
    _ <- IO("Still on main").debug
  } yield ()

  // Task 1:
  val invincibleAuthProgram: IO[Unit] = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(3.seconds) >> IO("Authenticating timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()

  // Task 2:
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable").debug >> IO.sleep(1.second) >> IO("cancellable end").debug) >>
        IO("uncancelable").debug >> IO.sleep(1.second) >> IO("uncancellable end") >>
        poll(IO("second cancelable").debug >> IO.sleep(1.second) >> IO("second cancellable end").debug)
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(1500.millis) >> IO("CANCELING").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] = threeStepProgram()
}
