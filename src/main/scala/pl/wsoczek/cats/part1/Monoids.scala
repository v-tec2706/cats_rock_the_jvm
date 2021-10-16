package pl.wsoczek.cats.part1

object Monoids extends App {

  import cats.instances.int._
  import cats.syntax.semigroup._

  val numbers: List[Int] = (1 to 1000).toList
  // |+| is always associative
  numbers.foldLeft(0)(_ |+| _) == numbers.foldRight(0)(_ |+| _)

  // we cannot write generic fold with only Semigroup as we don't know generic starting element (empty value)

  // Monoids - Semigroup but can also provide empty value

  import cats.Monoid

  val monoidInt = Monoid[Int]

  // Task 1: implement generic reduce by fold
  def combineFold[T: Monoid](list: List[T]): T = list.foldLeft(Monoid[T].empty)(_ |+| _)
  println(combineFold(List(2, 3, 4)))

  import cats.instances.option._
  println(combineFold(List(Some(2), None, Some(4)))) // Some(6)

  // Task 2: combine a list of phonebooks as Map[String, Int]
  val phonebooks = List(
    Map(
      "Alice" -> 123,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    )
  )

  println(combineFold(phonebooks))

  // Task 3: shopping cart and online stores with Monoids
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
    ShoppingCart(List.empty, 0.0),
    (a, b) => ShoppingCart(a.items ++ b.items, a.total + b.total)
  )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)
  println(checkout(List(ShoppingCart(List("Bread", "Butter"), 10.00), ShoppingCart(List("Eggs"), 4.00))))
}
