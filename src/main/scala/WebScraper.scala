
import cats.effect._
import cats.implicits._
import console._
import htmlPageParser._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.Uri
import org.http4s.client.blaze._
import org.http4s.dsl.io._

import scala.util.Either

// NOT FOR PRODUCTION
import scala.concurrent.ExecutionContext.Implicits.global

// TODO: save data to file
// TODO: collect product urls
object WebScraper {

  val pages: List[InverterRequestPage] = List(
    InverterRequestPage("fronius-galvo/fronius-galvo-1-5-1", "Fronius Galvo 1.5-1", "bacc_45e3b058-7e9d-4f21-a042-edb11b9efdd0_372160b4-7b89-436b-80c5-d56ae52046d5_"),
    InverterRequestPage("fronius-galvo/fronius-galvo-2-0-1", "Fronius Galvo 2.0-1", "bacc_04cb292b-0b0e-41ff-a44a-b3b4cc786cd9_372160b4-7b89-436b-80c5-d56ae52046d5_"),
    InverterRequestPage("fronius-galvo/fronius-galvo-2-5-1", "Fronius Galvo 2.5-1", "bacc_62e3e20e-25dd-42ab-9758-8029243cf005_372160b4-7b89-436b-80c5-d56ae52046d5_"),
    InverterRequestPage("fronius-galvo/fronius-galvo-3-0-1", "Fronius Galvo 3.0-1", "bacc_426411f5-b1bb-4747-9100-291b3fb0fb5d_372160b4-7b89-436b-80c5-d56ae52046d5_"),
    InverterRequestPage("fronius-galvo/fronius-galvo-3-1-1", "Fronius Galvo 3.1-1", "bacc_105735c3-5168-46d2-81c2-bf4f454157d9_372160b4-7b89-436b-80c5-d56ae52046d5_")
  )

  val program: IO[Unit] = for {
    httpClient <- Http1Client[IO]()
    _ <- Console[IO].println("Fetching Fronius Inverter pages")

    // fetching data from html pages
    pagesRawData <- fs2.async.parallelTraverse[List, IO, InverterRequestPage, (InverterRequestPage, Either[Throwable, List[InverterRawParameter]])](pages) {
      p =>
        val target = Uri.uri("http://www.fronius.com/en/photovoltaics/products/all-products/inverters/") / p.url
        for {
          _ <- Console[IO].println("Getting " + p.model)
          htmlPage <- httpClient.expect[String](target)
          _ <- Console[IO].println("Got " + p.model)
        } yield ((p, InverterPage.froniusInverterPage.parse(p.id)(htmlPage)))
    }

    _ <- Console[IO].println("Got all pages.")

    // converting response to json
    data <- fs2.async.parallelTraverse[List, IO, (InverterRequestPage, Either[Throwable, List[InverterRawParameter]]), Option[InverterData]](pagesRawData) { tuple =>
      tuple._2 match {
        case Left(err) => for {
          _ <- Console[IO].println("Could not parse inverter data, because of " + err.getMessage)
          _ <- IO(err.printStackTrace)
        } yield None
        case Right(v) =>
          val data = InverterData("Fronius", tuple._1.model, v)
          IO(Some(data))
      }
    }
    _ <- Console[IO].println("Here they are....")
    _ <- Console[IO].println(data.asJson.toString)

    _ <- Console[IO].println("Closing http client")
    _ <- httpClient.shutdown

    _ <- Console[IO].println("See you next time, Chao!")
  } yield ()

  def main(args: Array[String]): Unit = program.unsafeRunSync()
}

final case class InverterRequestPage(url: String, model: String, id: String)

final case class InverterData(manufacturer: String, model: String, rawParams: List[InverterRawParameter])