package catseffect.part3

import cats.effect.kernel.Deferred
import cats.effect.{IO, IOApp, Ref}
import cats.implicits.catsSyntaxParallelTraverse1
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt
import scala.util.Random

object CyclicBarriers extends IOApp.Simple {

  /*
    A cyclic barrier is a coordination primitive that:
     - is initialized with a count
     - has a single API: await

     A cyclic barrier will (semantically) block all fibers calling its await() method until we have exactly N fibers waiting,
     at which point the barrier will unblock all fibers and reset to its original state.
     Any further fiber will again block until we have exactly N fibers waiting.
   */

  override def run: IO[Unit] = openNetwork()

  def openNetwork(): IO[Unit] = for {
    _ <- IO(s"[announcer] Will start when there is 10 waiting threads").debug
    barrier <- CBarrier(10)
    _ <- (1 to 10).toList.parTraverse(id => createUser(id, barrier))
  } yield ()

  // example: signing up for a social network just about to be launched
  def createUser(id: Int, barrier: CBarrier): IO[Unit] = for {
    _ <- IO.sleep((Random.nextDouble() * 500).toInt.millis)
    _ <- IO(s"[user $id] waiting for remaining threads").debug
    _ <- IO.sleep((Random.nextDouble() * 1500).toInt.millis)
    _ <- barrier.await
    _ <- IO(s"[user $id] inside!").debug
  } yield ()

  // Task: Implement CB with Ref + Deferred
  abstract class CBarrier {
    def await: IO[Unit]
  }

  object CBarrier {

    def apply(count: Int): IO[CBarrier] = for {
      signal <- Deferred[IO, Unit]
      state <- Ref[IO].of[State](State(count, signal))
    } yield new CBarrier {
      override def await: IO[Unit] = for {
        newSignal <- Deferred[IO, Unit]
        _ <- state.modify {
          case State(1, signal) => State(count, newSignal) -> signal.complete(()).void
          case State(n, signal) => State(n - 1, signal) -> signal.get.void
        }.flatten
      } yield ()
    }

    case class State(count: Int, signal: Deferred[IO, Unit])
  }
}
