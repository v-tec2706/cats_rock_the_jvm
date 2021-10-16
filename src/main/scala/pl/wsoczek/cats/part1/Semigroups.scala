package pl.wsoczek.cats.part1

import cats.Semigroup
import cats.instances.int._

import java.time.LocalDate

object Semigroups extends App {
  // Semigroups combine elements of the same type
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 36) // addition

  println(intCombination)
  def listCombiner[T: Semigroup](l: List[T]): T = l.reduce(Semigroup[T].combine)

  val numbers = (1 to 10).toList
  val numberOptions = numbers.map(Option(_))

  import cats.instances.option._
  println(listCombiner(numberOptions)) // Some(55)

  val otherNumberOptions = List(Some(1), None, Some(2))
  println(listCombiner(otherNumberOptions)) // Some(3)

  // Task 1: Semigroup for custom type
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] {
    (e1, e2) => Expense(List(e1.id, e2.id).max, e1.amount + e2.amount)
  }

  val expenses = List(Expense(1, 10.0), Expense(2, 20.0))
  println(listCombiner(expenses)) // Expense(2, 30.0)

  // extension methods
  import cats.syntax.semigroup._
  import cats.instances.string._
  val strings = "1" |+| "2"

  // Task 2: reimplement listCombiner(...) us ing |+|
  def listCombiner2[T: Semigroup](l: List[T]): T = l.reduce(_ |+| _)
}