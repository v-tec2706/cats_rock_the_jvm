package catseffect.part1effects

import cats.Parallel
import cats.effect.IO.Par
import cats.effect.implicits.commutativeApplicativeForParallelF
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple {

  // IO are usually sequential
  import utils._
  import cats.syntax.apply._

  val foo: IO[String] = IO.delay("foo")
  val bar: IO[String] = IO.delay("bar")
  val both: IO[String] = (foo.debug, bar.debug).mapN((foo, bar) => s"both: $foo, $bar")

  // parallelism on IOs
  // convert sequential IO to parallel IO
  val parIO1: IO.Par[String] = Parallel[IO].parallel(foo.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(bar.debug)
  val bothPar: Par[String] = (parIO1, parIO2).mapN((foo, bar) => s"both: $foo, $bar")
  // turn back to sequential
  val bothParV2: IO[String] = Parallel[IO].sequential(bothPar)

  // shorthand:
  import cats.syntax.parallel._
  val bothParV3: IO[String] = (foo.debug, bar.debug).parMapN((foo, bar) => s"both: $foo, $bar")

  // failure
  val aFail: IO[String] = IO.raiseError(new RuntimeException("Error here!"))
  // compose success and failure
  val parallelWithFailure: IO[String] = (foo.debug, aFail.debug).parMapN(_ + _)

  // compose two failures
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Another failure"))
  val twoFailures: IO[String] = (aFail.debug, anotherFailure.debug).parMapN(_ + _)

  // the first effect to fail gives the final failure
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFail.debug, anotherFailure).parMapN(_ + _)

  override def run: IO[Unit] = twoFailuresDelayed.debug.void
}
