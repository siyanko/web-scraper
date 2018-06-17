
import cats.effect._
import cats.implicits._
import console._
import file._
import htmlPageParser._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.client.Client
import web._

import scala.concurrent.ExecutionContext
import scala.util.Either

object fronius_inverters {

  private def froniusInvertersDetailedPages[F[_] : Effect : HtmlPageCall](implicit ec: ExecutionContext): Client[F] => List[InverterRequestPage] => F[List[Option[(InverterRequestPage, String)]]] =
    httpClient => pages =>
      fs2.async.parallelTraverse[List, F, InverterRequestPage, Option[(InverterRequestPage, String)]](pages)(HtmlPageCall[F].fetchInverterDetailsPage(httpClient))

  private def inverterData[F[_] : Effect : Console](rawData: List[Option[(InverterRequestPage, ParsedPage[List[InverterRawParameter]])]])
                                                   (implicit ec: ExecutionContext): F[List[Option[InverterData]]] =
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


  def froniusInverters[F[_] : Effect : Console : FileStorage : HtmlPageCall]
  (implicit ec: ExecutionContext): Client[F] => F[Unit] = httpClient => for {

    // fetching list of products
    _ <- Console[F].println("Fetching Inverter list pages")
    pgs <- HtmlPageCall[F].fetchInverterListPage(httpClient, "http://www.fronius.com/en/photovoltaics/products")
    _ <- Console[F].println(s"Got ${pgs.size} pages")

    // prepend root url to the pages
    pages = pgs.map(p => p.copy(url = "http://www.fronius.com" + p.url))

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
    _ <- FileStorage[F].save("fronius_inverters.json", data.asJson.toString)
    _ <- Console[F].println("Data has been saved to file")
  } yield ()


}
