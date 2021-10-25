package pl.wsoczek.cats.part2



object States extends App {
  type MyState[S, A] = S => (S, A)

  // state: represents "iterative" computations
  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value

  // iterative vs FP
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"
  // vs
  val firstTransformation: State[Int, String] = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation: State[Int, String] = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))
  val composedTransformation: State[Int, (String, String)] = firstTransformation
    .flatMap(firstResult => secondTransformation.map(secondResult => (firstResult, secondResult)))
  // why not simply compose functions?
  // as we end up with nested tuples

  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")
  val compositeFunc = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }

  println(composedTransformation.run(10).value) // (55,(Added 1 to 10, obtained 11,Multiplied with 5, obtained 55))
  println(compositeFunc(10)) // (Added 1 to 10, obtained 11,(55,Multiplied with 5, obtained 55))

  // Task 1: an online store
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = State[ShoppingCart, Double]( s => (ShoppingCart(items = s.items :+ item, s.total + price), s.total + price))
  val myCart = for {
    _ <- addToCart("Guitar", 500)
    _ <- addToCart("Piano", 200)
    total <- addToCart("Violin", 300)
  } yield total

  println(myCart.run(ShoppingCart(List.empty, 0)).value)

  // Task 2: pure mental gymnastics
  // returns s State data structure that, when run, will not change the state but will issue the value f(a)
  def inspect[A, B](f: A => B): State[A, B] = State(s => (s, f(s)))
  // returns s State data structure that, when run, returns the value of that state and makes no changes
  def get[A]: State[A, A] = State(s => (s, s))
  // returns s State data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))
  // returns s State data structure that, when run, will return Unit and sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = State(s => (f(s), ()))

  // methods availiable in cats
  import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  println(program.run(10).value)
}
