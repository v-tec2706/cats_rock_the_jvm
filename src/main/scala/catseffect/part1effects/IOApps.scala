package catseffect.part1effects

import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.io.StdIn

object IOApps {
}

object TestApp {
  val program: IO[Unit] = for {
    line <- IO(StdIn.readLine())
    _ <- IO(println(s"Output: $line"))
  } yield ()

  def main(args: Array[String]): Unit = {
    program.unsafeRunSync()
  }
}
