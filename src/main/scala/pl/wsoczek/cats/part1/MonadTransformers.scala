package pl.wsoczek.cats.part1

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MonadTransformers extends App {

  // option transformer
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.list._

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  println(listOfTuples.value)

  // either transformers

  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(2)))

  /*
  Task 1:
   */

  import cats.instances.future._

  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170,
  )

  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT(Future[Either[String, Int]](Left(s"Server $server unreachable")))
    case Some(b) => EitherT(Future[Either[String, Int]](Right(b)))
  }

  // TODO 1: check if sum of server bandwidths is more than 250
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    b1 <- getBandwidth(s1)
    b2 <- getBandwidth(s2)
  } yield b1 + b2 > 250


  // TODO 2: print a string that describes if servers can handle load
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers cannot cope with spike: $reason")
      case Right(false) => Left("Servers cannot cope with spike")
      case Right(true) => Right("Servers are able to cope")
    }

  generateTrafficSpikeReport("server5.rockthejvm.com", "server2.rockthejvm.com").value.foreach(println)
}
