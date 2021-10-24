package pl.wsoczek.cats.part2

object Readers extends App {

  // use case
  /*
  We have an app where some services depends on configuration:
    config -> (dbLayer, HTTPLayer, businessLayer)
   */

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nTreads: Int, sttpProvider: String)
  case class DbConnection(userName: String,  password: String) {
    def getOrderStatus(orderId: Long) = s"dispatched $orderId"
    def getLastOrderId(userName: String): Long = 123123
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  // bootstrap
  val config = Configuration("admin", "admin", "localhost", 5006, 8, "some.mail@gmail.com")

  // cats reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn = dbReader.run(config)

  // Reader[I,O] allows to map over `O`
  val myOrderStatusReader: Reader[Configuration, String] = dbReader.map(_.getOrderStatus(55))
  val myOrderStatus: String = myOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): Reader[Configuration, String] =
    dbReader.map(_.getLastOrderId(username))
      .flatMap(orderId => dbReader.map(_.getOrderStatus(orderId)))


  /*
    Pattern
     1. you create initial data structure
     2. you create a reader which specifies how that data structure will be manipulated later
     3. you can then map & flatMap the reader to produce derived information
     4. when you need the final piece odf information, you call run on the reader within the initial data structure
   */

  println(getLastOrderStatus("witek").run(config))

  // Task 1: implement emailUser() method
  case class EmailService(sttpProvider: String) {
    def sendEmail(emailAddress: String, contents: String): Unit = println(s"[$emailAddress] $contents")
  }

  val emailreader = Reader[Configuration, EmailService](conf => EmailService(conf.sttpProvider))
  def emailUser(username: String, userEmail: String): Reader[Configuration, Unit] = for {
    lastOrderId <- dbReader.map(_.getLastOrderId(username))
    lastOrderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    _ <- emailreader.map(_.sendEmail(userEmail, s"Your order no. $lastOrderId reached status $lastOrderStatus and is read to ship"))
  } yield ()

  emailUser("someone", "someone@gmail.com").run(config)

  // Readers - purely functional way of dependency injecting
}
