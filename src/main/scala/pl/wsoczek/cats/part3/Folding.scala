package pl.wsoczek.cats.part3

import cats.{Eval, Monoid}
//import com.sun.tools.internal.xjc.reader.xmlschema.bindinfo.OptionalPropertyMode

object Folding extends App {
  // Task 1 - implement all methods form ListExercises in term of foldLeft
 object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldLeft(List.empty[B]) { (acc ,elem) => acc :+ f(elem) }
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldLeft(List.empty[B]) { (acc, elem) => acc ++ f(elem) }
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldLeft(List.empty[A]){ (acc, elem) => if (predicate(elem)) acc :+ elem else acc }
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = list.foldLeft(monoid.empty) { (acc, elem) => monoid.combine(acc, elem) }
  }

  import cats.Foldable
  import cats.instances.option._
  val someOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  // foldRight is stack-safe regardless of your container - because we use `Eval`
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1,2,3), Eval.now(1)) { (num, eval) =>
    eval.map(_ + num)
  }

  // convenience
  import cats.instances.int._ // Monoid[Int]
  val anotherSum = Foldable[List].combineAll(List(1,2,3)) // implicit Monoid[Int]

  import cats.instances.string._
  val mappedConcat = Foldable[List].foldMap(List(1,2,3))(_.toString)

  // nesting
  import cats.instances.vector._
  val intsNested = List(Vector(1,2,3), Vector(4,5,6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  // extension methods
  import cats.syntax.foldable._
  val sum3 = List(1,2,3).combineAll // requires: Foldable[List], Monoid[Int]


  // testing

  println(ListExercises.map((1 to 10).toList)(_ + 1))
  println(ListExercises.flatMap((1 to 10).toList)(e => List(e + 1)))

  case class C(v: Int)
  val m: Map[C, String] = Map.empty
  val s: Set[C] = Set.empty

//  println(s + C(1) + C(1))

  val m1 = Map(C(1) -> "1")
//  println(m1 + (C(1) -> "2"))
}
