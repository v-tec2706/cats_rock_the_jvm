package catseffect.part4

import cats.effect.kernel.Async
import cats.effect.{Concurrent, IO, IOApp, Temporal}

import scala.concurrent.duration.FiniteDuration

object PolymorphicTemporalSuspension extends IOApp.Simple {

  // Temporal - time-blocking effects
  trait MyTemporal[F[_]] extends Concurrent[F] {
    def sleep(time: FiniteDuration): F[Unit]
  }

  val temporalIO: Async[IO] = Temporal[IO]

  import cats.syntax.flatMap._
  // Task 1: generalize
  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)(implicit temporal: Temporal[F]): F[A] = {
    val sleepEffect = temporal.sleep(duration)
    temporal.race(fa, sleepEffect).flatMap {
      case Left(value) => temporal.pure(value)
      case Right(_) => temporal.raiseError(new RuntimeException("timeout of computation"))
    }
  }

  override def run: IO[Unit] = ???
}
