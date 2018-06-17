
import cats.effect._
import cats.implicits._
import console._
import file._
import ginlong_inverters._
import htmlPageParser._
import org.http4s.client.Client
import org.http4s.client.blaze._
import web._

object WebScraper {

  def program[F[_] : Effect : Console : FileStorage : HtmlPageCall](inverterProg: Client[F] => F[Unit]): F[Unit] = for {
    httpClient <- Http1Client[F]()

    _ <- inverterProg(httpClient)
    _ <- Console[F].println("Closing http client")
    _ <- httpClient.shutdown

    _ <- Console[F].println("See you next time, Chao!")
  } yield ()

  def main(args: Array[String]): Unit = program[IO](ginlongInverters).unsafeRunSync()
}

final case class InverterRequestPage(url: String, model: String)

final case class InverterData(manufacturer: String, model: String, rawParams: List[InverterRawParameter])