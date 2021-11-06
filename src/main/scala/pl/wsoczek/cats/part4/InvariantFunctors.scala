package pl.wsoczek.cats.part4

import cats.Monoid

object InvariantFunctors extends App {

  trait Crypto[A] {self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))
      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)
    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
   }

  val encrypted = encrypt("how are you")
  println(encrypted)
  println(decrypt[String](encrypted))

  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  println(encrypt(Math.PI))
  println(decrypt[Double](encrypt(Math.PI)))

  // Task 1: support Option[String]
  implicit val optionalStringCrypto: Crypto[Option[String]] = caesarCypher.imap[Option[String]](_.getOrElse(""), Option(_))

  // Task 2: if you have a Crypto[T] and a Monoid[T] in scope, find Crypto[Option[T]]
  implicit def optCrypto[T](implicit crypto: Crypto[T], monoid : Monoid[T]): Crypto[Option[T]] =
    crypto.imap[Option[T]](_.getOrElse(monoid.empty), Option(_))

  println(encrypt(Option(Math.PI)))
  println(decrypt[Option[Double]](encrypt(Math.PI)))

  import cats.Invariant
  import cats.Show
  import cats.instances.string._

  val showStrings = Show[String]
  val showOptionString = Invariant[Show].imap(showStrings)(Option(_))(_.getOrElse(""))

  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B) (back: B => A): W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[A])(back: B => A): W[B]
    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] {
    def map[A, B](wa: W[A])(forth: A => B): W[B]
    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = map(wa)(forth)
  }

 }
