package catseffect.part2

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}
import scala.util.Try

object AsyncIOs extends IOApp.Simple {

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool: ExecutorService = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(threadPool)
  type Callback[A] = Either[Throwable, A] => Unit

  def computeMeaningOfLifeInt(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing on some thread... ")
    42
  }

  def computeMeaningOfLife(): Either[Throwable, Int] = Try {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing on some thread... ")
    42
  }.toEither

  def computeOnThreadPool(): Unit =
    threadPool.execute(() => computeMeaningOfLife())

  // lift computation to an IO
  // async is a FFI(Foreign Function Interface)
  val asyncMolIO: IO[Int] = IO.async_ { cb => // CE thread blocks semantically until this cb is invoked by some other thread
    threadPool.execute { () => // computation not managed by CE
      val result = computeMeaningOfLife()
      cb(result) // CE thread is notified with the result
    }
  }

  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_[A] { (cb: Callback[A]) =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        cb(result)
      }
    }

  val asyncMol_v2: IO[Either[Throwable, Int]] = asyncToIO(computeMeaningOfLife)(ec)

  lazy val molFuture: Future[Int] = Future {
    computeMeaningOfLifeInt()
  }(ec)

  def convertFutureToIO[A](future: => Future[A]): IO[A] =
    IO.async_ { (cb: Callback[A]) =>
      future.onComplete { tryResult =>
        val result = tryResult.toEither
        cb(result)
      }
    }

  val asyncMolIO_v3: IO[Int] = IO.fromFuture(IO(molFuture))

  // Task: Create a never ending IO
  val neverEndingIO: IO[Int] = IO.async_[Int](_ => ()) // no callback = no finish
  val neverEndingIO_v2: IO[Int] = IO.never

  // Full async call
  val asyncMeaningOfLifeIO_v2: IO[Int] = IO.async { (cb: Callback[Int]) =>
  /*
  finalizer in case computation gets cancelled
  finalizers are of type IO[Unit]
  not specifying finalizer => Option[IO[Unit]]
  creating option is an effect => IO[Option[IO[Unit]]]
   */
    IO {
      threadPool.execute { () =>
        val result = computeMeaningOfLife()
        cb(result)
      }
    }.as(Some(IO("Cancelled!").debug.void)) // <- this option is called on computations cancellation
  }

  val demo: IO[Unit] = for {
    fib <- asyncMeaningOfLifeIO_v2.start
    _ <- IO.sleep(500.millis) >> IO("Cancelling...").debug >> fib.cancel
    _ <- fib.join
  } yield ()

  override def run: IO[Unit] = demo.debug >> IO(threadPool.shutdown())
}
