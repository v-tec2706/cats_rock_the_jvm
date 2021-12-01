package catseffect.part4

import cats.effect.kernel.Async
import cats.effect.{Concurrent, Deferred, Fiber, FiberIO, IO, IOApp, Outcome, OutcomeIO, Ref, Spawn}
import catseffect.part3.Mutex
import catseffect.part4.PolymorphicCancellation.unsafeSleep
import utils.general.DebugWrapper

import scala.collection.immutable.Queue
import scala.concurrent.duration.DurationInt

object PolymorphicCoordination extends IOApp.Simple {

  // Concurrent - Ref + Deferred for ANY effect type
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]
    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO: Async[IO] = Concurrent[IO]
  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]


//  def alarmNotifier(): IO[Unit] = {
//    def incrementer(counter: Ref[IO, Int], signal: Deferred[IO, Int]): IO[Unit] = for {
//      currentValue <- counter.updateAndGet(_ + 1)
//      _ <- IO(s"current value is: $currentValue").debug
//      _ <- if (currentValue == 10) signal.complete(currentValue) else IO.unit
//      _ <- IO.sleep(1.second)
//      _ <- incrementer(counter, signal)
//    } yield ()
//
//    def listener(signal: Deferred[IO, Int]): IO[Unit] = for {
//      _ <- IO("waiting for proper value").debug
//      value <- signal.get
//      _ <- IO(s"Got value: $value").debug
//    } yield ()
//
//    for {
//      counter <- Ref[IO].of(0)
//      signal <- Deferred[IO, Int]
//      incrementRef <- incrementer(counter, signal).start
//      listener <- listener(signal).start
//      _ <- incrementRef.join
//      _ <- listener.join
//    } yield ()
//  }

  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.effect.syntax.spawn._

  def polymorphicAlarmNotifier[F[_]](implicit concurrent: Concurrent[F]): F[Unit] = {
    def incrementer(counter: Ref[F, Int], signal: Deferred[F, Int]): F[Unit] = for {
      currentValue <- counter.updateAndGet(_ + 1)
      _ <- concurrent.pure(s"current value is: $currentValue").debug2
      _ <- if (currentValue == 10) signal.complete(currentValue) else concurrent.unit
      _ <- unsafeSleep(1.second)
      _ <- incrementer(counter, signal)
    } yield ()

    def listener(signal: Deferred[F, Int]): F[Unit] = for {
      _ <- concurrent.pure("waiting for proper value").debug2
      value <- signal.get
      _ <- concurrent.pure(s"Got value: $value").debug2
    } yield ()

    for {
      counter <- concurrent.ref(0)
      signal <- concurrent.deferred[Int]
      incrementRef <- incrementer(counter, signal).start
      listener <- listener(signal).start
      _ <- incrementRef.join
      _ <- listener.join
    } yield ()
  }

  // Task 1: Generalize racePair
  // Task 2: Generalize mutex
  type RaceResult[F[_], A, B] = Either[
    (Outcome[F, Throwable, A], Fiber[F, Throwable, B]),
    (Fiber[F, Throwable, A], Outcome[F, Throwable, B])]

  type EitherOutcome[F[_], A, B] = Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]

  def ourRacePair[F[_], A, B](ioa: F[A], iob: F[B])(implicit concurrent: Concurrent[F]): F[RaceResult[F, A, B]] = concurrent.uncancelable { poll =>
    for {
      signal <- concurrent.deferred[EitherOutcome[F, A, B]]
      fiba <- concurrent.guaranteeCase(ioa)(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibb <- concurrent.guaranteeCase(iob)(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- concurrent.onCancel(
        poll(signal.get), {
          for {
            cancelFibA <- fiba.cancel.start // to have them cancelled at the same time
            cancelFibB <- fibb.cancel.start
            _ <- cancelFibA.join
            _ <- cancelFibB.join
          } yield ()
        }
      ) // blocking - should be cancelable
      toReturn = result match {
        case Left(outcomeA) => Left((outcomeA, fibb))
        case Right(outcomeB) => Right((fiba, outcomeB))
      }
    } yield toReturn
  }

  abstract class Mutex[F[_]] {
    def acquire: F[Unit]

    def release: F[Unit]
  }

  object Mutex {
    type Signal[F[_]] = Deferred[F, Unit]

    case class State[F[_]](locked: Boolean, waiting: Queue[Signal[F]])

    def unlocked[F[_]]: State[F] = State[F](locked = false, Queue())

    def createSignal[F[_]]()(implicit concurrent: Concurrent[F]): F[Deferred[F, Unit]] = concurrent.deferred[Unit]

    def create[F[_]](implicit concurrent: Concurrent[F]): F[Mutex[F]] = concurrent.ref(unlocked[F]).map(createMutexWithCancellation[F])

    def createMutexWithCancellation[F[_]](state: Ref[F, State[F]])(implicit concurrent: Concurrent[F]): Mutex[F] = new Mutex[F] {
      /*
      Change the state of the Ref:
       - if the mutex is currently unlocked, state becomes (true, [])
       - if the mutex is locked, state becomes (true, queue + new signal) and block on that signal
       */
      override def acquire: F[Unit] = concurrent.uncancelable { poll =>
        createSignal().flatMap { (signal: Deferred[F, Unit]) =>

          val cleanup = state.modify {
            case State(locked, waiting) =>
              val newQueue = waiting.filterNot(_ eq signal)
              State(locked, newQueue) -> release
          }.flatten

          state.modify {
            case State(false, _) => State[F](locked = true, Queue()) -> concurrent.unit
            case State(true, queue) => State[F](locked = true, queue.enqueue(signal)) -> concurrent.onCancel(poll(signal.get), cleanup)
          }.flatten
        }
      }

      /*
      Change the state of the Ref:
       - if the mutex is unlocked, leave the state unchanged
       - if the mutex is locked,
         - if the queue is empty, unlock the mutex, i.e state becomes (false, [])
         - if the queue is not empty, take a signal out of the queue and complete it
       */
      override def release: F[Unit] = state.modify {
        case State(false, _) => unlocked[F] -> concurrent.unit
        case State(true, queue) =>
          if (queue.isEmpty) unlocked[F] -> concurrent.unit
          else {
            val (signal, rest) = queue.dequeue
            State[F](locked = true, rest) -> signal.complete(()).void
          }
      }.flatten
    }
  }

  override def run: IO[Unit] = polymorphicAlarmNotifier[IO]
}
