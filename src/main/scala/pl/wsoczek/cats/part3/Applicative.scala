package pl.wsoczek.cats.part3

object Applicatives extends App {

  // Applicative = functors (map) + the pure method

  import cats.Applicative
  import cats.instances.list._

  val listsApplicative = Applicative[List]
  val aList = listsApplicative.pure(2)

  import cats.instances.option._

  val optionApplicative = Applicative[Option]
  val option = optionApplicative.pure(2)

  // pure extensions method

  import cats.syntax.applicative._
  import cats.instances.option._

  val aSweetList = 2.pure[List]
  val aSweetOption = 2.pure[Option]

  // Monads extend Applicatives
  // Applicative extends Functors

  import cats.data.Validated

  type ErrorOr[T] = Validated[List[String], T]
  val aValidValue: ErrorOr[Int] = Validated.valid(42) // pure
  val aModifiedValidated: ErrorOr[Int] = aValidValue.map(_ + 1)
  val aValidatedApplicative = Applicative[ErrorOr]

  // Validated is not a Monad, its strongest TC is Applicative

  //  Task 1:
  def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B] = ???

  def productWithApplicatives[F[_], A, B](fa: F[A], fb: F[B])(implicit applicative: Applicative[F]): F[(A, B)] = {
    //applicative.map(fa)(a => applicative.map(fb)(b => (a, b)) )
    val functionWrapper = applicative.map(fa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(fb)
  }

  println(productWithApplicatives(List(1,2,3), List(3,4,5)))
  // Applicatives has this `def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B] = ???`
  // Applicatives can implement product from Semigroupal
  // => Applicatives extends Semigroupal
}

