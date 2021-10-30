package pl.wsoczek.cats.part2

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Writers extends App {

  import cats.data.Writer

  // with this writer we can operate on Int value and keep some additional information
  // in List[String]
  // Like Option[Int] but with some additional info

  //   1. Define Writer at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)
  //   2. Manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting") // value stays same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something interesting", _ + 1)
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) => (logs :+ s"found something interesting: $value", value + 1) } // we can access values in both structures

  // flatMap

  import cats.instances.vector._ // imports a Semigroup[Vector] to combine logs

  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  println(compositeWriter) // WriterT((Vector(Log A1, Log A2, Log B1),50))

  // reset the logs

  import cats.instances.list._ // a Monoid[List[Int]] - to get empty List via `reset`

  val anEmptyWriter = aWriter.reset // clear the logs, keep the value

  //   3. Dump either the value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written

  // Task 1: implement `countAndLog(...)` = rewrite `countAndSay(...)` in FP way
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting!"), 0)
    else countAndLog(n - 1).mapBoth { (vec, _) => (vec.appended(s"$n"), n) }
  }

  // Benefit #1: we work with pure FP, no side effects
  countAndLog(10).written.toList.foreach(println)

  // Task 2: rewrite `naiveSum(...)` with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed  sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithWriter(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector.empty[String], 0)
    else {
      sumWithWriter(n - 1).mapBoth { (logs, value) =>
        (logs.prepended(s"Now at $n").appended(s"computed sum (${n - 1}): $value"), n + value)}
    }
  }

  naiveSum(5)
  println(sumWithWriter(5).written)

  Future(naiveSum(100)).foreach(println)
  Future(naiveSum(100)).foreach(println)
  // vs
  val sumFuture1 = Future(sumWithWriter(100))
  val sumFuture2 = Future(sumWithWriter(100))
  val logs1 = sumFuture1.map(_.written)
  val logs2 = sumFuture2.map(_.written)
  // Benefit #2: we can separate logs from different threads
}
