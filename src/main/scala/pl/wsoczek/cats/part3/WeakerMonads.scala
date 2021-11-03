package pl.wsoczek.cats.part3

import cats.{Applicative, Apply}

object WeakerMonads extends App {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A=> M[B]): M[B]

    // Task 1: Implement `ap`
    // hint: Apply extends Functor
    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wf)(f => map(wa)(a => f(a)))
  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._ // flatMap extension method
  import cats.syntax.functor._ // map extension method


}
