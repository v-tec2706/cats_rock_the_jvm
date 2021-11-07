package part1effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global

object IOIntroduction extends App {

  // IO
  val intIO: IO[Int] = IO.pure(42)
  val delayedIO: IO[Int] = IO.delay {
    println("Doing something")
    44
  }

  val anotherDelayedIO: IO[Int] = IO {
    println("Also doing something")
    33
  }

  // mapN - combine IO effects as tuples

  import cats.syntax.apply._

  val combinedMeaningOfLife = (delayedIO, anotherDelayedIO).mapN(_ + _)

  println(delayedIO.unsafeRunSync())

  // Exercises

  // 1: sequence IOs and take the result of the last one
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)

  // 2: sequence two IOs and take the result of the FIRST one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    for {
      resultIoa <- ioa
      _ <- iob
    } yield resultIoa

  // == ioa <* iob

  // 3: repeat an IO effect forever
  def forever[A](io: IO[A]): IO[A] = io.flatMap(_ => forever(io))
  def forever_v2[A](io: IO[A]): IO[A] = io >> forever_v2(io)
  def forever_v3[A](io: IO[A]): IO[A] = io *> forever_v3(io)
  def forever_v4[A](io: IO[A]): IO[A] = io.foreverM // with tail recursion

  // forever(IO(println("forever!"))).unsafeRunSync()
  // forever_v3(IO(println("forever!"))).unsafeRunSync()

  // 4: convert an IO to different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa.map(_ => value)

  // 5: discard value inside an IO, just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] = ioa.map(_ => ())

  // 6: fix stack recursion
  def sum(n: Int): Int =
    if (n == 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n == 0) IO(0)
    else IO(n).flatMap(n => sumIO(n - 1).map(prev => prev + n))

  println(sumIO(20000).unsafeRunSync())
//  println(sum(20000))

  // 7 - write Fibonacci IO that does not crash on recursion
  def fibonacci(n: Int): IO[BigInt] =
    if (n == 0 || n == 1) IO(1)
    else for {
      last <- IO.defer(fibonacci(n-1)) // == .delay(...).flatten
      prev <- IO.defer(fibonacci(n-2))
    } yield last + prev

  println(fibonacci(10).unsafeRunSync())
}
