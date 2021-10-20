package pl.wsoczek.cats.part1

object MonadsPractice extends App {

  import cats.Monad

  // either as a monad
  val aManualEither: Either[String, Int] = Right(42)
  type ErrorOr[T] = Either[String, T]

  import cats.instances.either._

  val errorMonad = Monad[ErrorOr]
  val anEither = errorMonad.pure(12)

  // Task 1: the service layer API of a web app
  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  /* Requirements:
   1. M[_] is for example Option
   2. if host and port are found in config map, return M with connection, otherwise fail
   3. issueRequest returns a M[_] containing the string "request accepted: (payload)" if payload is less than 20, else i fails
  */

  val myHttpService = new HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        requiredHost <- cfg.get("host")
        requiredPort <- cfg.get("port")
      } yield Connection(requiredHost, requiredPort)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length > 20) None else Some(s"Returning payload: $payload")
  }




}
