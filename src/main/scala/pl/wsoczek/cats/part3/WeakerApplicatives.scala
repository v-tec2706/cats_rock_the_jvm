package pl.wsoczek.cats.part3

import cats.{Functor, Semigroupal}

object WeakerApplicatives extends App {
  // Apply - weaker form of applicatives
  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    // Task 1: define mapN(...)
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper) {
        case (a, b) => f(a, b)
      }
    }

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T] // fundamental
  }

  trait MyApplicative[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A] // fundamental
  }

  import cats.Apply
  import cats.instances.option._

  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  import cats.syntax.apply._
  // Apply is used for:
  val tupledOfOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupledOfOptions.tupled // Some(1,2,3)
  val sumOption = tupledOfOptions.mapN(_ + _ + _) // Some(6)
}
