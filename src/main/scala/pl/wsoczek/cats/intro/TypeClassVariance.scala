package pl.wsoczek.cats.intro

object TypeClassVariance {

  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val aComparison: Boolean = Option(2) === Option(3)
  // val anInvalidComparison = Some(2) === None !! Eq[Some[Int]] not found, because Eq is invariant so

  // variance - generic type annotation that allows to propagate subtyping to higher types
  // rule of thumb: "HAS a T" (contains T, eq. List, Option) = covariant, "ACTS on T" (operates on it) = contravariant
  class Animal
  class Cat extends Animal

  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // covariant

  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // contravariant

  // variance affect how TC instances are being fetched

  // contravariant TC
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("sound made")
  makeSound[Animal]
  makeSound[Cat] // works because compiler search for SoundMaker[Cat], having SoundMaker contravariant means SoundMaker[Animal] <: SoundMaker[Cats], so we have it
  // rule 1: contravariant TCs can use the superclass instances if nothing is available strictly for that type

  // so, having:
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]] // now, it works!


  // convariant TC
  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "cats"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show
  // rule 2: covariant TCs will always use the more specific TC instance for that type but may confuse the compiler

  println(organizeShow[Cat]) // ok
  //  println(organizeShow[Animal]) !! doesn't compile as we have ambiguous implicits, both for cat and animal

}
