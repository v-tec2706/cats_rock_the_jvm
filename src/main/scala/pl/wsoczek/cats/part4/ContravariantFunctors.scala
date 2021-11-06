package pl.wsoczek.cats.part4

import cats.Show
import cats.kernel.Monoid


object ContravariantFunctors extends App {

  trait Format[T] { self =>
    def format(value: T): String
    def contrmap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]) = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  // problem: given Format[MyType], can we have a Format[Option[MyType]]?
  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] = f.contrmap[Option[T]](_.getOrElse(m.empty))

  println(format(42))
  println(format(Option(42)))

   /*
   compilers infers types from simplest to most complex:
    1) IntFormat
    2) fo1: Format[Option[Int]] = IntFormat.contrmap[Option[Int]])(_.get) // first get
    3) fo2: Format[Option[Option[Int]] = fo1.contrmap[Option[Option[Int]](_.get) // second get

    fo2 = IntFormat
      .contrmap[Option[Int]])(_.get) // first get
      .contrmap[Option[Option[Int]](_.get) // second get

     Order of operations:
     fo2.format(Option(Option(42)) =
       fo1.format(secondGet(Option(Option(42))) =
       IntFormat.format(firstGet(secondGet(Option(Option(42))))

      order:
       - second get
       - first get
       - format of Int

       Map applies transformations in sequence they were written
       Contramap executes operation in reverse order they were written

    */

  import cats.Contravariant
  import cats.instances.int._ // implicit Show[Int]

  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))
}
