package catseffect.part4

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp, MonadCancel, Spawn}
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt

object PolimorphicFibers extends IOApp.Simple {

  // Spawn = create fibers for any effect
  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E] {
    def start[A](fa: F[A]): F[Fiber[F, Throwable, A]] // create a fiber
    def never[A]: F[A] // a forever-suspending effect
    def cede: F[Unit] // a "yield" effect

    def racePair[A, B](fa: F[A], fb: F[B]): Either[
      (Outcome[F, E, A], Fiber[F, E, B]),
    (Fiber[F, E, A], Outcome[F, E, B])]
  }

  trait MySpawn[F[_]] extends MyGenSpawn[F, Throwable]

  val spawnIO = Spawn[IO] // fetch the implicit Spawn[IO]

  def ioOnSomeThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- spawnIO.start(io) // io.start assumes the presence of a Spawn[IO]
    result <- fib.join
  } yield result


  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def effectOnSomeThread[F[_], A](fa: F[A])(implicit spawn: Spawn[F]): F[Outcome[F, Throwable, A]] = for {
    fib <- spawn.start(fa)
    result <- fib.join
  } yield result

  // Task: Generalize the following code
  def simpleRace[F[_], A, B](ioa: F[A], iob: F[B])(implicit spawn: Spawn[F]): F[Either[A, B]] =
    spawn.racePair(ioa, iob).flatMap {
      case Left((outA, slowFib)) => outA match {
        case Succeeded(fa) => slowFib.cancel >> fa.map[Either[A, B]](Left(_))
        case Errored(e) => slowFib.cancel >> spawn.raiseError[Either[A, B]](new RuntimeException(s"Execution failed: $e"))
        case Canceled() => slowFib.join.flatMap {
          case Succeeded(effectB) => effectB.map(Right(_))
          case Errored(e) => spawn.raiseError(e)
          case Canceled() => spawn.raiseError(new RuntimeException("Both IOs cancelled"))
        }
      }

      case Right((slowFib, outB)) => outB match {
        case Succeeded(fa) => slowFib.cancel >> fa.map[Either[A, B]](Right(_))
        case Errored(e) => slowFib.cancel >> spawn.raiseError[Either[A, B]](new RuntimeException(s"Execution failed: $e"))
        case Canceled() => slowFib.join.flatMap {
          case Succeeded(effectB) => effectB.map(Left(_))
          case Errored(e) => spawn.raiseError(e)
          case Canceled() => spawn.raiseError(new RuntimeException("Both IOs cancelled"))
        }
      }
    }

  val fast: IO[Int] = IO.sleep(1.second) >> IO(42).debug
  val slow: IO[Int] = IO.sleep(2.second) >> IO(45).debug

  val race: IO[Either[Int, Int]] = simpleRace(fast, slow)


  override def run: IO[Unit] = race.void
}
