import cats.effect._
import console._
import htmlPageParser._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.client.blaze._
import org.http4s.dsl.io._

object WebScraper {

  val program: IO[Unit] = for {
    httpClient <- Http1Client[IO]()
    _ <- Console[IO].println("Fetching Fronius Galvo 2.0-1 page")
    page <- httpClient.expect[String]("http://www.fronius.com/en/photovoltaics/products/all-products/inverters/fronius-galvo/fronius-galvo-2-0-1")
    _ <- Console[IO].println("GOT Fronius Galvo 2.0-1 page")
    bodyElements = InverterPage.froniusInverterPage.parse(page)
    _ <- Console[IO].println(bodyElements.asJson.toString)
    _ <- Console[IO].println("")
    _ <- Console[IO].println("Closing http client")
    _ <- httpClient.shutdown
    _ <- Console[IO].println("See you next time, Chao!")
  } yield ()

  def main(args: Array[String]): Unit = program.unsafeRunSync()
}
