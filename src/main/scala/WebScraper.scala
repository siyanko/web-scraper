import cats.effect._
import io.circe.generic.auto._
import io.circe.syntax._
import org.http4s.client.blaze._
import org.http4s.dsl.io._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import console._

import scala.collection.JavaConverters._

object WebScraper {

  type Page = String
  type ParameterKey = String
  type ParameterValue = String

  final case class InverterRawParameter(key: String, value: String)

  //unsafe
  def  parseTable(table: Element): List[InverterRawParameter] = {
    table.select("tr").asScala.toList
      .flatMap(tr => tr.children().asScala.toList.map(_.text()).sliding(2).map{
        case List(a, b) => InverterRawParameter(a, b)
      }.toList)
  }


  //unsafe
  def productBlock(p: Page): List[InverterRawParameter]= {
    val doc = Jsoup.parse(p)
    val body = doc.body()
//    body.getElementById("bacc_45e3b058-7e9d-4f21-a042-edb11b9efdd0_372160b4-7b89-436b-80c5-d56ae52046d5_") // fronius galvo 1.5-1
    body.getElementById("bacc_04cb292b-0b0e-41ff-a44a-b3b4cc786cd9_372160b4-7b89-436b-80c5-d56ae52046d5_") // fronius galvo 2.0-1
      .getElementsByTag("table")
      .asScala.toList
      .flatMap(parseTable)
  }

  val program: IO[Unit] = for {
    httpClient <- Http1Client[IO]()
    _ <- Console[IO].println("Fetching Fronius Galvo 2.0-1 page")
    page <- httpClient.expect[String]("http://www.fronius.com/en/photovoltaics/products/all-products/inverters/fronius-galvo/fronius-galvo-2-0-1")
    _ <- Console[IO].println("GOT Fronius Galvo 2.0-1 page")
    bodyElements = productBlock(page)
    _ <- Console[IO].println(bodyElements.asJson.toString)
    _ <- Console[IO].println("")
    _ <- Console[IO].println("Closing http client")
    _ <- httpClient.shutdown
    _ <- Console[IO].println("See you next time, Chao!")
  } yield ()

  def main(args: Array[String]): Unit = program.unsafeRunSync()
}
