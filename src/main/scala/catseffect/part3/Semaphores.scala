package catseffect.part3

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import utils.DebugWrapper
import cats.syntax.parallel._

import scala.concurrent.duration.DurationInt
import scala.util.Random

object Semaphores extends IOApp.Simple {
  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) // 2 total permits

  def doingSomething: IO[Int] = IO.sleep(1.second) >> IO(Random.nextInt(100))

  def login(id: Int, sem: Semaphore[IO]): IO[Unit] = for {
    _ <- IO(s"[session-$id] waiting to log in...").debug
    _ <- sem.acquire
    _ <- IO(s"[session-$id] logged in, working...").debug
    res <- doingSomething
    _ <- IO(s"[session-$id] done, logging out...").debug
    _ <- sem.release
  } yield res

  def demoSemaphore(): IO[Unit] = for {
    sem <- Semaphore[IO](2)
    user1Fib <- login(1, sem).start
    user2Fib <- login(2, sem).start
    user3Fib <- login(3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  def weightedLogin(id: Int, requiredPermits: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"[session-$id] waiting to log in...").debug
    _ <- sem.acquireN(requiredPermits)
    _ <- IO(s"[session-$id] logged in, working...").debug
    res <- doingSomething
    _ <- IO(s"[session-$id] done, logging out...").debug
    _ <- sem.releaseN(requiredPermits)
  } yield res

  def demoWeightedSemaphore(): IO[Unit] = for {
    sem <- Semaphore[IO](2)
    user1Fib <- weightedLogin(1, 1, sem).start
    user2Fib <- weightedLogin(2, 2, sem).start
    user3Fib <- weightedLogin(3, 3, sem).start
    _ <- user1Fib.join
    _ <- user2Fib.join
    _ <- user3Fib.join
  } yield ()

  override def run: IO[Unit] = users.void

  // Task 1: Semaphore(1) == Mutex
  val mutex: IO[Semaphore[IO]] = Semaphore[IO](1)
  val users: IO[List[Int]] = mutex.flatMap { sem =>
    (1 to 10).toList.parTraverse { id =>
      for {
        _ <- IO(s"[session-$id] waiting to log in...").debug
        _ <- sem.acquire
        _ <- IO(s"[session-$id] logged in, working...").debug
        res <- doingSomething
        _ <- IO(s"[session-$id] done, logging out...").debug
        _ <- sem.release
      } yield res
    }
  }
}
