package pl.wsoczek.cats.part1

import scala.util.Try

object Functors extends App {

  // Functor - type class that generalises `map` method
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._
  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1,2,3))(_ + 1)

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generic way
  def do10x[F[_]: Functor](container: F[Int]): F[Int] = Functor[F].map(container)(_ * 10)

  println(do10x(Option(132)))

  // Task 1: define functor for binary tree
  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Leaf(value) => Leaf(f(value))
        case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
      }
  }

  val tree: Tree[Int] = Branch[Int](10, Leaf[Int](5), Leaf[Int](30))
  val mapped =TreeFunctor.map(tree)(_ + 1)
  println(mapped)

  // extension method - map
  import cats.syntax.functor._

  tree.map(_ + 1)
  def do10xv2[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)

}
