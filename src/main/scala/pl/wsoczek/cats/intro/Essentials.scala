package pl.wsoczek.cats.intro

object Essentials extends App {

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  def showAny[A](a: A)(implicit p: Print[A]): String = p.show(a)

  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  trait Print[A] {
    def show(a: A): String
  }

  implicit def oneArgClass[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}" : "${value.productElement(0)}"}
         |""".stripMargin
  }

  case class Cat(name: String)

  listToJson(List(Cat("Garfield"), Cat("Tom")))

  implicit def toPrintable[A <: Product]: Print[A] = new Print[A] {
    override def show(a: A): String = a.toString
  }

  case class MyCaseClass(elem: Int)

}
