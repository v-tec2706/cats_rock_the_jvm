package pl.wsoczek.cats.part4

object Kleislies extends App {

  // Kleisli = wrapper over functions returning higher kinded instances, used for function composition
  // having plain function
  val plainFunc1: Int => String = x => if (x % 2 == 0) "ok" else "no ok"
  val plainFunc2: Int => Int = x => x + 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  // having functions with results wrapped:
  val func1: Int => Option[String] = x => if (x % 2 == 0) Some("ok") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

  // val func3: Int => Option[String] = func2 andThen func2 // do not work
  // we cannot compose them - Kiesili solves this problem

  import cats.data.Kleisli
  import cats.instances.option._

  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience
  val multiply = func2K.map(_ * 2) // x => Option(...).map(_ * 2)
  val chain = func2K.flatMap(x => func1K)

  // Task 1
  import cats.Id
  type InterestingKleisili[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]

  val times2 = Kleisli[Id, Int, Int](x => x * 2)
  val plus4 = Kleisli[Id, Int, Int](y => y + 4)
  val com posed = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composedFor = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  println(composedFor(3)) // 13

//  Kleisli[Id, Int, Int] == Reader[Int, Int]
}
