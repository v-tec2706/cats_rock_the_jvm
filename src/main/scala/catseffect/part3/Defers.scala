package catseffect.part3

import cats.effect.kernel.Deferred
import cats.effect.{FiberIO, IO, IOApp, OutcomeIO, Ref, Resource}
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt
import cats.syntax.traverse._

object Defers extends IOApp.Simple {

  // deferred is a primitive for waiting for an effect, while some other effect completes with a value
  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int]

  // `get(...)` blocks the calling fiber (semantically) until some other fiber completes the Deferred with a value
  val reader: IO[Int] = aDeferred.flatMap { signal =>
    signal.get
  }

  val writer: IO[Boolean] = aDeferred.flatMap { signal =>
    signal.complete(42)
  }

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]) = for {
      _ <- IO(s"[consumer] waiting for result ...").debug
      value <- signal.get // blocker
      _ <- IO(s"[consumer] got the result $value").debug
    } yield ()

    def producer(signal: Deferred[IO, Int]) = for {
      _ <- IO(s"[producer] crunching numbers...").debug
      _ <- IO.sleep(1.second)
      _ <- IO(s"[producer] completed 42").debug
      _ <- signal.complete(42)
    } yield ()

    for {
      signal <- Deferred[IO, Int]
      fibConsumer <- consumer(signal).start
      fibProducer <- producer(signal).start
      _ <- fibProducer.join
      _ <- fibConsumer.join
    } yield ()
  }

  // simulate downloading some content
  val fileParts = List("These ar", "e lon", "g senten", "ce parts!<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] =
      fileParts.map { part =>
        IO(s"got '$part'").debug >> IO.sleep(1.second) >> contentRef.update(currentContent => currentContent + part)
      }.sequence.void

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = for {
      file <- contentRef.get
      _ <- if (file.endsWith("<EOF>")) IO("File download complete").debug
      else IO("[notifier] downloading...").debug >> notifyFileComplete(contentRef)
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      fibDownloader <- downloadFile(contentRef).start
      notifier <- notifyFileComplete(contentRef).start
      _ <- fibDownloader.join
      _ <- notifier.join
    } yield ()

    // bad - busy waiting
  }

  def fileNotifierWithDeferred(): IO[Unit] = {
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO("[notifier] downloading...").debug
      _ <- signal.get // blocks until signal  is completed
      _ <- IO("[notifier] File download complete").debug
    } yield ()

    def fileDownloader(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] = for {
      _ <- IO(s"[downloader] got '$part'").debug
      _ <- IO.sleep(1.second)
      latestContent <- contentRef.updateAndGet(currentContent => currentContent + part)
      _ <- if (latestContent.contains("<EOF>")) signal.complete(latestContent) else IO.unit
    } yield ()

    for {
      contentRef <- Ref[IO].of("")
      signal <- Deferred[IO, String]
      notifierFib <- notifyFileComplete(signal).start
      fileTasksFib <- fileParts.map(part => fileDownloader(part, contentRef, signal)).sequence.start
      _ <- notifierFib.join
      _ <- fileTasksFib.join
    } yield ()
  }

  /* Task 1:
      - write a small alarm notification with two simultaneous IOs
        - one that increments a counter every second (a clock)
        - one that waits for the counter to become 10
   */

  def alarmNotifier(): IO[Unit] = {
    def incrementer(counter: Ref[IO, Int], signal: Deferred[IO, Int]): IO[Unit] = for {
      currentValue <- counter.updateAndGet(_ + 1)
      _ <- IO(s"current value is: $currentValue").debug
      _ <- if (currentValue == 10) signal.complete(currentValue) else IO.unit
      _ <- IO.sleep(1.second)
      _ <- incrementer(counter, signal)
    } yield ()

    def listener(signal: Deferred[IO, Int]): IO[Unit] = for {
      _ <- IO("waiting for proper value").debug
      value <- signal.get
      _ <- IO(s"Got value: $value").debug
    } yield ()

    for {
      counter <- Ref[IO].of(0)
      signal <- Deferred[IO, Int]
      incrementRef <- incrementer(counter, signal).start
      listener <- listener(signal).start
      _ <- incrementRef.join
      _ <- listener.join
    } yield ()
  }

  /* Task 2:
  implement racePair with Deferred
   - use a deferred which can hold an Either[outcome for ioa, outcome for iob]
   - start two fibers, one for each IO
   - on completion (with any status), each IO needs to complete that Deferred
     (use finalizer, use a guarantee call to make sure the fibers complete the Deferred)
    - what do you do in case of cancellation?
   */

  type RaceResult[A, B] = Either[
    (OutcomeIO[A], FiberIO[B]),
    (FiberIO[A], OutcomeIO[B])]

  type EitherOutcome[A, B] = Either[OutcomeIO[A], OutcomeIO[B]]

  def ourRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, EitherOutcome[A, B]]
      fiba <- ioa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibb <- iob.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- poll(signal.get).onCancel {
        for {
          cancelFibA <- fiba.cancel.start // to have them cancelled at the same time
          cancelFibB <- fibb.cancel.start
          _ <- cancelFibA.join
          _ <- cancelFibB.join
        } yield ()
      } // blocking - should be cancelable
      toReturn = result match {
        case Left(outcomeA) => Left((outcomeA, fibb))
        case Right(outcomeB) => Right((fiba, outcomeB))
      }
    } yield toReturn
  }

  override def run: IO[Unit] = alarmNotifier()
}
