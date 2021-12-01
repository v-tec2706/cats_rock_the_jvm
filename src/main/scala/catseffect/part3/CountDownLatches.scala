package catseffect.part3

import cats.effect.std.CountDownLatch
import cats.effect.{IO, IOApp, Resource}
import cats.implicits.{catsSyntaxParallelTraverse1, toTraverseOps}
import utils.DebugWrapper

import java.io.{File, FileWriter}
import scala.concurrent.duration.DurationInt
import scala.io.Source

object CountDownLatches extends IOApp.Simple {

  /*
      CountDownLatches (CDL) are a coordination primitive initialized with a count.
      All fibers calling await() on the CDL are (semantically) blocked.
      When the internal count of the latch reaches 0 (via release() calls from other fibers), all waiting fibers are unblocked.
   */

  def announcer(latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO(s"Starting race shortly...").debug >> IO.sleep(2.seconds)
    _ <- IO(s"5...").debug >> IO.sleep(1.seconds)
    _ <- latch.release
    _ <- IO(s"4...").debug >> IO.sleep(1.seconds)
    _ <- latch.release
    _ <- IO(s"3...").debug >> IO.sleep(1.seconds)
    _ <- latch.release
    _ <- IO(s"2...").debug >> IO.sleep(1.seconds)
    _ <- latch.release
    _ <- IO(s"1...").debug >> IO.sleep(1.seconds)
    _ <- latch.release
    _ <- IO("Starting!").debug
  } yield ()

  def createRunner(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO(s"[runner $id] waiting for signal...").debug
    _ <- latch.await // block this fiber until the count reaches 0
    _ <- IO(s"[runner $id] RUNNING!").debug
  } yield ()

  def sprint(): IO[Unit] = for {
    latch <- CountDownLatch[IO](5)
    announcerFib <- announcer(latch).start
    _ <- (1 to 10).toList.parTraverse(id => createRunner(id, latch))
    _ <- announcerFib.join
  } yield ()

  // Task 1: simulate a file downloader on multiple threads
  object FileServer {
    val fileChunksList: Array[String] = Array(
      "foo",
      "bar",
      "sth else"
    )

    def getNumChunks: IO[Int] = IO(fileChunksList.length)

    def getFileChunk(n: Int): IO[String] = IO(fileChunksList(n))


    def writeToFile(path: String, contents: String): IO[Unit] = {
      val fileResource = Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
      fileResource.use { writer =>
        IO(writer.write(contents))
      }
    }

    def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
      val compositeResource = for {
        reader <- Resource.make(IO(Source.fromFile(fromPath)))(source => IO(source.close()))
        writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
      } yield (reader, writer)

      compositeResource.use {
        case (reader, writer) => IO(reader.getLines().foreach(writer.write))
      }
    }

    private def fileDownloadJob(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
      fileContent <- getFileChunk(id)
      _ <- writeToFile(s"chunks/chunk-$id.txt", fileContent)
      _ <- latch.release
    } yield fileContent

    def downloadFile(filename: String, destinationFolder: String): IO[Unit] = for {
      nChunks <- getNumChunks
      count <- CountDownLatch[IO](nChunks - 1)
      _ <- (0 until nChunks).toList.parTraverse(fileDownloadJob(_, count)).start
      _ <- count.await
      _ <- (0 until nChunks).toList.traverse(id => appendFileContents(s"chunks/chunk-$id.txt", "chunks/all.txt"))
    } yield ()
  }

  override def run = FileServer.downloadFile("", "").void

}
