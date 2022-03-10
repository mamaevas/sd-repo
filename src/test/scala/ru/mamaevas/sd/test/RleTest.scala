package ru.mamaevas.sd.test

import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import com.typesafe.scalalogging.LazyLogging
import org.scalatest.Succeeded
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.language.implicitConversions
import scala.util.Random

class RleTest extends AsyncFlatSpec with Matchers with LazyLogging {

  import RLE._

  private val system = ActorSystem("RleTestSystem")
  implicit val ec: ExecutionContextExecutor = system.dispatcher
  implicit val mat: Materializer = Materializer(system)

  private def initData(max: Int): Future[String] =
    Source(1 to max)
      .grouped(1000)
      .mapAsync(10) { seq =>
        Future.sequence {
          seq.map { _ =>
            Future((Random.nextInt(2) + 65).toChar)
          }
        }
      }
      .runFold(List.empty[Char])(_ ++ _)
      .map(_.mkString)

  "rleEncode" should "be fine" in {
    "".rleEncode shouldBe ""
    "aabcc".rleEncode shouldBe "a2bc2"
    "abc".rleEncode shouldBe "abc"
    "abbc".rleEncode shouldBe "ab2c"
    "abba".rleEncode shouldBe "ab2a"
  }

  "rleParEncode" should "be fine" in {
    for {
      _ <- "".rleParEncode().map(_ shouldBe "")
      _ <- "aabcc".rleParEncode().map(_ shouldBe "a2bc2")
      _ <- "abc".rleParEncode().map(_ shouldBe "abc")
      _ <- "abbc".rleParEncode().map(_ shouldBe "ab2c")
      _ <- "abba".rleParEncode().map(_ shouldBe "ab2a")
    } yield Succeeded
  }

  "rleEncodePerformance" should "be fine with basic method and be fast with parallel execution" in {
    for {
      _ <- Future.unit
      data <- initData(100 * 1000)
      _ = preHeating(data)
      _ <- Future(Thread.sleep(2000))

      parStart = System.currentTimeMillis()
      parStr <- data.rleParEncode(parallelism = 10)
      parRes = System.currentTimeMillis() - parStart
      _ = logger.info(s"Parallel execution time: ${parRes} ms")
      //      _ <- parStr.map(s => logger.debug(s))


      start = System.currentTimeMillis()
      str <- Future(data.rleEncode)
      res = System.currentTimeMillis() - start
      _ = logger.info(s"Sequential execution time: ${res} ms")
      //      _ = logger.debug(str)

      _ = parStr shouldBe str
      _ = parRes should be < res
    } yield Succeeded
  }

  private def preHeating(data: String): Future[Unit] =
    for {
      _ <- Future.unit
      _ = logger.info("Start preheating")
      _ <- Future(data.rleEncode)
      _ <- data.rleParEncode()
      _ = logger.info("End preheating")
    } yield ()
}
