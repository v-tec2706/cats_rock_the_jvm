package pl.wsoczek.cats.part1

object Monads extends App {

  // lists
  val numbersList = List(1,2,3)
  val charsList = List('a', 'b', 'c')

  // Task 1: How to create all combinations of (number, chars)?
  val combinations1 = for {
    number <- numbersList
    char <- charsList
  } yield (number, char)

  val combinations2: List[(Int, Char)] = numbersList.flatMap(number => charsList.map(char => (number, char)))

  println(combinations1)
  println(combinations2)

  // options
  val numberOption = Option(2)
  val charOption = Option('d')

  numberOption.flatMap(n => charOption.map(c => (n, c)))

  // Cats Monad
  import cats.Monad
  import cats.instances.option._

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(1)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if(x % 2 == 0) Some(x + 1) else None)

  // generalized zipping API
  def getPairs[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = Monad[M].flatMap(ma)(a => Monad[M].map(mb)(b => (a, b)))
  println(getPairs(Option(12), Option('1')))

  // extension methods
  import cats.syntax.applicative._ // pure

  val oneOption = 1.pure[Option]
  val oneList = 1.pure[List]

  // Task 2: implement `map`
  trait MyMonad[M[_]] {
    def pure[A](value : A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  // Monad extends Functors

  // Task 3: write `getPairs(...)` shorter
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  def getPairsFor[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = for {
    a <- ma
    b <- mb
  } yield (a, b)

  println(getPairsFor(Option(11), Option(12)))
}

