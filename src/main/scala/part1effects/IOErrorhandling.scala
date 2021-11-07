package part1effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global

object IOErrorhandling extends App {
  // create failed effects
  val aFailedCompute = IO.delay(throw new RuntimeException("Failure"))
  val aFailure = IO.raiseError(new RuntimeException("a proper fail"))

  // turn into an Either
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  // redeem: transform the failure and the success in one go
  val resultAsString: IO[String] = aFailure.redeem(err => s"Some error: $err", value => s"Ok: $value")

  println(resultAsString.unsafeRunSync())
}
