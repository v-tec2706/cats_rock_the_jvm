package part1effects

import cats.effect.{IO, IOApp}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  def heavyComputation(s: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    s.length
  }

  val workload: List[String] = List("foo", "bar123", "both foo and bar")

  def clunkyFutures(): Unit = {
    val futures = workload.map(heavyComputation)
    futures.foreach(_.foreach(println))
  }

  import cats.Traverse
  import cats.instances.list._

  val listTraverse: Traverse[List] = Traverse[List]

  def traverseFutures(): Unit = {
    // traverse

    val singleFuture: Future[List[Int]] = listTraverse.traverse(workload)(heavyComputation)
    // ^^ this stores ALL the results
    singleFuture.foreach(println)
  }

  import utils._
  // traverse for IO
  def computeAsIO(s: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    s.length
  }.debug

  val ios: List[IO[Int]] = workload.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workload)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._
  val parallelSingleIO: IO[List[Int]] = workload.parTraverse(computeAsIO)

  // Task 1: implement sequence(...) method
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(identity)

  def sequence2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(identity)

  // existing sequence API
  val singleIOv2: IO[List[Int]] = listTraverse.sequence(ios)

  override def run: IO[Unit] = singleIO.void
}
