package part1effects

import scala.io.StdIn

object Effects extends App {
  /* pure functional programming

   referential transparency = can replace an expression with its value as many time
                               as we want

   effects examples:
      - print to console
      - change a variable

   side effects are inevitable for useful programs

   Effect  = bridges necessity of creating effects with functional programming
     Properties:
        - type signature describes the kind of calculation that will be preformed
        - type signature describes the VALUE that will be calculated
        - when side effects are needed, effect construction is separate from effect execution
   */

  /*
    example 1: Option is an effect type
      - describes a possibly absent value
      - computes a value of type A, if it exists
      - side effects are not needed

    example 2: Future is NOT an effect type
     - describes an asynchronous computation
     - computes a value of type A, if it's successful
     - side effect is required (allocating/scheduling a thread), execution is NOT separate from construction
   */

  /*
   example 3: custom monad class
      - describes any computation that might produce side effects
      - calculates a value of type A, if it's successful
      - side effects are required for the evaluation of () => A
         - the creation of MyIO does NOT produce the side effects on construction
   */

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  /*
    Exercises:
      1. Create an MyIO which returns current time of the system
      2. Implement measure(...) method
      3. Print sth to console
      4. Read a line from std input
   */

  // 1
  val currentTime = MyIO(() => System.currentTimeMillis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[Long] =
    for {
      start <- currentTime
      _ <- computation
      end <- currentTime
    } yield end - start

  // 3
  val p = MyIO(() => println("Something"))

  // 4
  val r = MyIO(() => StdIn.readLine())
}
