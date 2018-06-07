
import cats.effect._
import cats.implicits._
import console._
import file._
import htmlPageParser._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.blaze._
import org.http4s.dsl.io._

import scala.util.Either

// NOT FOR PRODUCTION
import scala.concurrent.ExecutionContext.Implicits.global

object WebScraper {

  val froniusInverterList: Client[IO] => IO[List[InverterRequestPage]] = httpClient => for {
    htmlPage <- httpClient.expect[String]("http://www.fronius.com/en/photovoltaics/products")
    _ <- Console[IO].println("Got page.")
    _ <- Console[IO].println("Parsing the page.")
    pages <- HtmlPageParser.parse[String, List[InverterRequestPage]](htmlPage) match {
      case Left(err) => for {
        _ <- Console[IO].println("Could not parse inverters page: " + err.getMessage)
        _ <- IO(err.printStackTrace())
      } yield List.empty[InverterRequestPage]

      case Right(pages) => IO(pages)
    }
  } yield pages

  val froniusInvertersDetailedPages: Client[IO] => List[InverterRequestPage] => IO[List[(InverterRequestPage, String)]] =
    httpClient => pages =>
    fs2.async.parallelTraverse[List, IO, InverterRequestPage, (InverterRequestPage, String)](pages) {
      p =>
        val target = Uri.uri("http://www.fronius.com") / p.url
        for {
          _ <- Console[IO].println("Getting " + p.model)
          htmlPage <- httpClient.expect[String](target)
          _ <- Console[IO].println("Got " + p.model)
        } yield (p, htmlPage)
    }

  val inverterData: List[(InverterRequestPage, ParsedPage[List[InverterRawParameter]])] => IO[List[Option[InverterData]]] = rawData =>
    fs2.async.parallelTraverse[List, IO, (InverterRequestPage, Either[Throwable, List[InverterRawParameter]]), Option[InverterData]](rawData) { tuple =>
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


  val program: IO[Unit] = for {
    httpClient <- Http1Client[IO]()

    // fetching list of products
    _ <- Console[IO].println("Fetching Fronius Inverter list pages")
    pages <- froniusInverterList(httpClient)
    _ <- Console[IO].println(s"Got ${pages.size} pages")

    // fetching data from html pages
    _ <- Console[IO].println("Fetching Fronius Inverter pages")
    detailedPages <- froniusInvertersDetailedPages(httpClient)(pages)
    _ <- Console[IO].println("Got all pages.")

    // processing pages data
    pagesRawData = detailedPages.map {
      case (requestPage, responsePage) => (requestPage, HtmlPageParser.parse[String, List[InverterRawParameter]](responsePage))
    }
    data <- inverterData(pagesRawData)
    _ <- Console[IO].println(s"Got ${data.size} items")

    _ <- Console[IO].println("Saving data to file")
    _ <- FileStorage[IO].save("fronius_inverters.json", data.asJson.toString)
    _ <- Console[IO].println("Data has been saved to file")

    _ <- Console[IO].println("Closing http client")
    _ <- httpClient.shutdown

    _ <- Console[IO].println("See you next time, Chao!")
  } yield ()

  def main(args: Array[String]): Unit = program.unsafeRunSync()
}

final case class InverterRequestPage(url: String, model: String)

final case class InverterData(manufacturer: String, model: String, rawParams: List[InverterRawParameter])