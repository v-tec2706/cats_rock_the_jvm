package pl.wsoczek.cats.part2

import scala.annotation.tailrec

/*
  Cats makes the distinction between:
  - evaluation an expression eagerly
  - evaluating lazily and every time you request it
  - evaluation lazily and keeping the value (memoizing)
 */

object Evaluation extends App {

  import cats.Eval

  // executed at initialization, only once
  val instantEval = Eval.now {
    println("Computing now!")
    6434
  } // Computing now!
  println(instantEval) // Now(6434)
  println(instantEval) // Now(6434)

  // executed each time
  val redoVal = Eval.always {
    print("Computing again!")
    4234
  }

  println(redoVal.value) // Computing again!4234
  println(redoVal.value) // Computing again!4234

  // evaluated later and only once
  val delayedEval = Eval.later {
    print("Computing later!")
    54334
  }

  println(delayedEval.value) // Delayed computation54334
  println(delayedEval.value) //54334

  // "remember" a computed value
  val dontRecompute = redoVal.memoize

  // Task 1: implement defer such that defer(Eval.now) does NOT run side effects
  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)

  defer(Eval.now {
    println("Now!")
    42
  })

  // Task 2: rewrite `reverseList(...)` the method with Evals
  def reverseList[T](list: List[T]): List[T] = {
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head
  }

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else reverseEval(list.tail).map(l => l :+ list.head)


  println(reverseEval(List(1,2,3)).value)

  // using expression like Eval.later(...).flatMap(... =>) we get stack safe solution as `later()`
  // is implemented with tailrec helpers
}

