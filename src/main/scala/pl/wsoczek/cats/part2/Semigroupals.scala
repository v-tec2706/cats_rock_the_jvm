package pl.wsoczek.cats.part2

import cats.Monad

object Semigroupals extends App {

  // Semigroupal - tuple values regardless how they where computed
   trait MySemigroupals[F[_]] {
     def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
   }

  import cats.Semigroupal
  import cats.instances.option._

  val optionalSemigroupal = Semigroupal[Option]
  val aTupledOption = optionalSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneOption = optionalSemigroupal.product(Some(123), None) // None

  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(List(1,2), List("a", "b"))
  println(aTupledList) // List((1,a), (1,b), (2,a), (2,b))

  // Task 1: implement product with monads
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  println(productWithMonads(List(1,2), List("a", "b"))) // List((1,a), (1,b), (2,a), (2,b)
  // Monads extends Semigroupals
  // Semigroupals pay no attention to operation ordering in contrast to monads. Useful for example for: Validated

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validateSemigroupal = Semigroupal[ErrorsOr] // requires Semigroup to combine items, Semigroup[List[_]]

  val invalidCombinations = validateSemigroupal.product(
    Validated.invalid(List("Something wrong")),
    Validated.invalid(List("Something else wrong"))
  )

  println(invalidCombinations) // Invalid(List(Something wrong, Something else wrong))
  // Associativity:  m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._
  val eitherSemigroupal = Semigroupal[EitherErrorsOr] // in terms of map/flatmap
  val eitherCombination = eitherSemigroupal.product(
    Left(List("Something wrong")),
    Left(List("Something else wrong"))
  )

  println(eitherCombination) // Left(List(Something wrong))

  import cats.data.Validated
  // Task 2: define a Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa zip fb
  }
}
