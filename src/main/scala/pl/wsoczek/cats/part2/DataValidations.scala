package pl.wsoczek.cats.part2

import pl.wsoczek.cats.part2.DataValidations.FormValidation.validateFrom

object DataValidations extends App {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(42)
  val anInvalidValue: Validated[String, Int] = Validated.invalid("something went wrong")
  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "meaning of lide is too small")

  // Task 1: use Either
  /*
     - n must be a prime
     - n must be non-negative
     - n <= 100
     - n must be even
   */
  // interoperate with stdlib
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(42))

  def testNumber(n: Int): Either[List[String], Int] = {
    val conditions = List(((n: Int) => n > 0, "must be positive"), ((n: Int) => n < 100, "must be less than 100"), ((n: Int) => n % 2 == 0, "must be even"))
    val errors = conditions.map { case (f, s) => (f(n), s) }.collect { case (false, s) => s }
    if (errors.isEmpty) Right(n) else Left(errors)
  }

  println(testNumber(333))
  println(validatedNumber(333))

  // chain
  aValidValue.andThen(_ => anInvalidValue)
  // test valid value
  aValidValue.ensure(List("something wronf"))(_ % 2 == 0)
  // transform
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)

  def validatedNumber(n: Int): Validated[List[String], Int] =
    Validated.cond(n % 2 == 0, n, List("must be even"))
      .combine(Validated.cond(n >= 0, n, List("must be positive")))
      .combine(Validated.cond(n < 300, n, List("must be less than 300")))

  // Option, Try ...

  // Task 2: form validation

  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    /*
    fields are:
     - name
     - email
     - password

     rules are:
      - name, email and password must be specified
      - name must not be blank
      - email must have "@"
      - password must have >= 10 chars
     */


    def getValue(value: Option[String], fieldName: String): FormValidation[String] =
      Validated.fromOption(value, List(s"field: $fieldName - must not be empty"))

    def isNotBlank(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty , value, List(s"field: $fieldName cannot be blank"))

    def isValidEmail(value: String): FormValidation[String] =
      Validated.cond(value.contains('@'), value, List("email must containt `@`"))

    def validateFrom(form: Map[String, String]): FormValidation[String] =
      getValue(form.get("name"), "name").andThen(isNotBlank(_, "name"))
        .combine(getValue(form.get("email"), "email").andThen(isValidEmail))
  }

  println(validateFrom(Map("email" -> "asdfasdfasdf", "password" -> "sdf14123123sdfsdfsdf")))

}
