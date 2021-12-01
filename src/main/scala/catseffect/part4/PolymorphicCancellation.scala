package catseffect.part4

import cats.effect.kernel.Async
import cats.{Applicative, Monad}
import cats.effect.{IO, IOApp, MonadCancel, Poll}
import utils.DebugWrapper
import utils.utils2.DebugWrapper

import scala.concurrent.duration.{DurationInt, FiniteDuration}

object PolymorphicCancellation extends IOApp.Simple {
  override def run: IO[Unit] = ???

  trait MyApplicativeError[F[_], E] extends Applicative[F] {
    def raiseError[A](error: E): F[A]
    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  // MonadCancel

  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]
    def uncancelable[A](poll: Poll[F] => F[A]): F[A]
  }

  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]


  // Task: Generalize a piece of code
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import cats.instances.string._

//  def unsafeSleep[F[_], E](duration: FiniteDuration)(implicit mc: MonadCancel[F, E]): F[Unit] =
//    mc.pure(Thread.sleep(duration.toMillis))
//
//  def inputPassword[F[_], E](a: String)(implicit monadCancel: MonadCancel[F, E]): F[String] = for {
//    _ <- monadCancel.pure("Input password: ").debug2
//    _ <- monadCancel.pure("(typing password)").debug2
//    _ <- unsafeSleep(2.seconds)
//    aa <- monadCancel.pure("password123!!")
//  } yield aa
//
//  def verifyPassword[F[_], E](password: String)(implicit monadCancel: MonadCancel[F, E]): F[Boolean] = for {
//    _ <- monadCancel.pure("verifying...").debug2
//    _ <- unsafeSleep(2.seconds)
//    _ <- monadCancel.pure(true)
//  } yield ()
//
//  def authFlow[F[_], E](implicit monadCancel: MonadCancel[F, E]): F[Unit] = monadCancel.uncancelable { poll =>
//    for {
//      pw <- poll(inputPassword).onCancel(monadCancel.pure("Authentication timed out").debug2.void) // is cancellable
//      verified <- verifyPassword(pw)
//      _ <- if (verified) monadCancel.pure("Authentication successful").debug2
//      else monadCancel.pure("Authentication failed").debug2
//    } yield ()
//  }
//
//  val authProgram: IO[Unit] = for {
//    authFib <- authFlow[IO, Throwable].start
//    _ <- IO.sleep(3.seconds) >> IO("Authenticating timeout, attempting cancel...").debug >> authFib.cancel
//    _ <- authFib.join
//  } yield ()
}
