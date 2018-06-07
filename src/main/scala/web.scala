import cats.effect.IO
import console.Console
import htmlPageParser.HtmlPageParser
import org.http4s.Uri
import org.http4s.client.Client

object web {

  trait HtmlPageCall[F[_]] {

    def fetchInverterListPage(httpClient: Client[F], url: String): F[List[InverterRequestPage]]

    def fetchInverterDetailsPage(httpClient: Client[F])(pages: InverterRequestPage): F[Option[(InverterRequestPage, String)]]
  }

  object HtmlPageCall {
    def apply[F[_]](implicit F: HtmlPageCall[F]): HtmlPageCall[F] = F

    implicit val HtmlPageCallIO: HtmlPageCall[IO] = new HtmlPageCall[IO] {
      override def fetchInverterListPage(httpClient: Client[IO], url: String): IO[List[InverterRequestPage]] = for {
        htmlPage <- httpClient.expect[String](url)
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

      override def fetchInverterDetailsPage(httpClient: Client[IO])(page: InverterRequestPage): IO[Option[(InverterRequestPage, String)]] =
        Uri.fromString(page.url) match {
          case Left(err) => for {
            _ <- Console[IO].println(s"Invalid url: ${page.url}, details: ${err.details}")
          } yield None

          case Right(target) => for {
            _ <- Console[IO].println("Getting " + page.model)
            htmlPage <- httpClient.expect[String](target)
            _ <- Console[IO].println("Got " + page.model)
          } yield Some((page, htmlPage))
        }
    }

  }

}
