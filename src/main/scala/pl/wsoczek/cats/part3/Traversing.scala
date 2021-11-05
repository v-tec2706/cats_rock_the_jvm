package pl.wsoczek.cats.part3

import cats.{Applicative, Foldable, Functor, Monad}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Traversing extends App {

  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 20)

  /*
  Problem to solve:
   1. We have:
    - a List[String] (servers)
    - a func String => Future[Int] (getBandwidth)
   2. We want a Future[List[Int]]
   */

  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (acc, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidths <- acc
      band <- bandFuture
    } yield accBandwidths :+ band
  }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // Task 1: implement method
  // bad solution
  def listTraverseBad[F[_] : Monad, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.map(func).foldLeft(Monad[F].pure(List.empty[B])) { (f, elem) => Monad[F].flatMap(f)(l => Monad[F].map(elem)(e => l :+ e)) }

  import cats.syntax.applicative._
  import cats.syntax.apply._

  // nice solution
  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (acc, elem) => (acc, func(elem)).mapN(_ :+ _) }

  // Task 2: implement method
  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse[F, F[A], A](list)(identity)

  import cats.instances.vector._

  listSequence(List(Vector(1, 2), Vector(3, 4))) // Vector(List(1,3), List(1,4), List(2,3), List(2,4)) - all possible pairs

  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))

  val allTrue = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2,4,6)
  val someFalse = filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]

  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2, 4, 6)
  val someFalseValidated = filterAsValidated(List(1, 2, 3))(_ % 2 == 0) // Invalid(List("predicate for 1", "predicate for 3"))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_] : Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    // Task 3: implement map using traverse and/or sequence
    import cats.Id
    def map[A, B](wa: L[A])(f: A => B): L[B] = traverse[Id, A, B](wa)(f)
  }

  import cats.Traverse

}
