
import cats.effect._
import cats.implicits._
import console._
import file._
import htmlPageParser._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.client.Client
import org.http4s.client.blaze._
import web._

import scala.util.Either

// NOT FOR PRODUCTION
import scala.concurrent.ExecutionContext.Implicits.global

object WebScraper {

  def froniusInvertersDetailedPages[F[_] : Effect : HtmlPageCall]: Client[F] => List[InverterRequestPage] => F[List[Option[(InverterRequestPage, String)]]] =
    httpClient => pages =>
      fs2.async.parallelTraverse[List, F, InverterRequestPage, Option[(InverterRequestPage, String)]](pages)(HtmlPageCall[F].fetchInverterDetailsPage(httpClient))

  def inverterData[F[_] : Effect : Console](rawData: List[Option[(InverterRequestPage, ParsedPage[List[InverterRawParameter]])]]): F[List[Option[InverterData]]] =
    fs2.async.parallelTraverse[List, F, Option[(InverterRequestPage, Either[Throwable, List[InverterRawParameter]])], Option[InverterData]](rawData) {
      _ match {
        case None => Effect[F].pure(None)
        case Some((reqPage, maybeInverterRawParams)) => maybeInverterRawParams match {
          case Left(err) => for {
            _ <- Console[F].println("Could not parse inverter data, because of " + err.getMessage)
            _ <- Effect[F].pure(err.printStackTrace)
          } yield None
          case Right(v) =>
            val data = InverterData("Fronius", reqPage.model, v)
            Effect[F].pure(Some(data))
        }
      }
    }

  def httpClient[F[_] : Effect]: F[Client[F]] = Http1Client[F]()

  def shutDownHttpClient[F[_]](cl: Client[F]): F[Unit] = cl.shutdown

  def program[F[_] : Effect : Console : FileStorage : HtmlPageCall](rootUrl: String, productsUrl: String, filePath: String): F[Unit] = for {
    httpClient <- httpClient[F]

    // fetching list of products
    _ <- Console[F].println("Fetching Inverter list pages")
    pgs <- HtmlPageCall[F].fetchInverterListPage(httpClient, productsUrl)
    _ <- Console[F].println(s"Got ${pgs.size} pages")

    // prepend root url to the pages
    pages = pgs.map(p => p.copy(url = rootUrl + p.url))

    // fetching data from html pages
    _ <- Console[F].println("Fetching Inverter pages")
    detailedPages <-
      fs2.async.parallelTraverse[List, F, InverterRequestPage, Option[(InverterRequestPage, String)]](pages)(HtmlPageCall[F].fetchInverterDetailsPage(httpClient))
    _ <- Console[F].println(s"Got ${detailedPages.size} pages.")

    // processing pages data
    pagesRawData = detailedPages.map(_.map {
      case (requestPage, responsePage) => (requestPage, HtmlPageParser.parse[String, List[InverterRawParameter]](responsePage))
    })
    data <- inverterData(pagesRawData)
    _ <- Console[F].println(s"Got ${data.size} items")

    // save data to file
    _ <- Console[F].println("Saving data to file")
    _ <- FileStorage[F].save(filePath, data.asJson.toString)
    _ <- Console[F].println("Data has been saved to file")

    _ <- Console[F].println("Closing http client")
    _ <- shutDownHttpClient[F](httpClient)

    _ <- Console[F].println("See you next time, Chao!")
  } yield ()

  def main(args: Array[String]): Unit =
    program[IO](
      "http://www.fronius.com",
      "http://www.fronius.com/en/photovoltaics/products",
      "fronius_inverters.json")
      .unsafeRunSync()
}

final case class InverterRequestPage(url: String, model: String)

final case class InverterData(manufacturer: String, model: String, rawParams: List[InverterRawParameter])