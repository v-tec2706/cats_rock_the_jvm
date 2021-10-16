package pl.wsoczek.cats.intro

import cats.Eq

object CatsEq extends App {

  import cats.syntax.eq._

  // Eq - enforces we compare only types of the same type
  val intEq: Eq[Int] = Eq[Int]
  intEq.eqv(23, 33)
  "123123" === "12"

  import cats.instances.list._

  val aListComparison: Boolean = List("asd") === List("aaasdasd")
  println(aListComparison)

  // Eq type class instance for custom types
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (c1, c2) => c1.price == c2.price }
}
