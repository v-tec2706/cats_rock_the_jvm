package pl.wsoczek.cats.part3

import cats.Applicative

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

object HandlingErrors extends App {
  import cats.Monad

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    // pure from Applicative
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M]{
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either

  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success = monadErrorEither.pure(32) // Etiher[String, Int] == Right(32)
  val failure = monadErrorEither.raiseError[Int]("something wrong") // Either[String, Int] == Left("something wrong")

  // recover
  val handleError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "badness" => 44
    case _ => 89
  }

  // recoverWith (takes another ErrorOr)
  val handlingError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure){
    case "Badness" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _ => Left("Something else") // ErrorOr[Int]
  }

  // filter
  val filterSuccess = monadErrorEither.ensure(success)("number too small")(_ > 100)

  // Try and Future
  import cats.instances.try_._ // implicit MonadError[Try], E = Throwable
  val exception = new RuntimeException("Really bad")
  val pureException = MonadError[Try, Throwable].raiseError(exception) // Failure(exception)

  import cats.instances.future._
  MonadError[Future, Throwable].raiseError(exception)

  // applicatives => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List] = ApplicativeError[ErrorOr, List[String]]
  type ErrorsOr[T] = Validated[List[String], T]

  import cats.ApplicativeError
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]]

  // pure, raiseError, handleError, handleErrorWith


  // extension methods
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._
  val extendedSuccess = 42.pure[ErrorsOr]
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  val recoverError = extendedError.recover {
    case _ => 43
  }

  import cats.syntax.monadError

}
