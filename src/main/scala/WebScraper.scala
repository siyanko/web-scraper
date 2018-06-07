
import cats.effect._
import cats.implicits._
import console._
import file._
import htmlPageParser._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.Uri
import org.http4s.client.blaze._
import org.http4s.dsl.io._

import scala.util.Either

// NOT FOR PRODUCTION
import scala.concurrent.ExecutionContext.Implicits.global

// TODO: collect product urls
object WebScraper {

  val pages: List[InverterRequestPage] = List(
    InverterRequestPage("fronius-galvo/fronius-galvo-1-5-1", "Fronius Galvo 1.5-1"),
    InverterRequestPage("fronius-galvo/fronius-galvo-2-0-1", "Fronius Galvo 2.0-1"),
    InverterRequestPage("fronius-galvo/fronius-galvo-2-5-1", "Fronius Galvo 2.5-1"),
    InverterRequestPage("fronius-galvo/fronius-galvo-3-0-1", "Fronius Galvo 3.0-1"),
    InverterRequestPage("fronius-galvo/fronius-galvo-3-1-1", "Fronius Galvo 3.1-1")
  )

  val program: IO[Unit] = for {
    httpClient <- Http1Client[IO]()
    _ <- Console[IO].println("Fetching Fronius Inverter pages")

    // fetching data from html pages
    // TODO: make web module
    pagesRawData <- fs2.async.parallelTraverse[List, IO, InverterRequestPage, (InverterRequestPage, Either[Throwable, List[InverterRawParameter]])](pages) {
      p =>
        val target = Uri.uri("http://www.fronius.com/en/photovoltaics/products/all-products/inverters/") / p.url
        for {
          _ <- Console[IO].println("Getting " + p.model)
          htmlPage <- httpClient.expect[String](target)
          _ <- Console[IO].println("Got " + p.model)
        } yield ((p, HtmlPageParser.froniusHtmlPageParser.parse(htmlPage)))
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
    _ <- Console[IO].println(s"Got ${data.size} items")

    _ <- Console[IO].println("Saving data to file")
    _ <- FileStorage[IO].save("test.json", data.asJson.toString) 
    _ <- Console[IO].println("Data has been saved to file")       

    _ <- Console[IO].println("Closing http client")
    _ <- httpClient.shutdown

    _ <- Console[IO].println("See you next time, Chao!")
  } yield ()

  def main(args: Array[String]): Unit = program.unsafeRunSync()
}

final case class InverterRequestPage(url: String, model: String)

final case class InverterData(manufacturer: String, model: String, rawParams: List[InverterRawParameter])