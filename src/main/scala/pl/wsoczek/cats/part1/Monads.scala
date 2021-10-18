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

}

