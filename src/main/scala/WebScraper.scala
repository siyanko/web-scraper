import cats.effect._
import org.http4s.client.blaze._
import org.http4s.dsl.io._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import scala.collection.JavaConverters._

object WebScraper {

  type Page = String
  type ParameterKey = String
  type ParameterValue = String


  //unsafe
  def  parseTable(table: Element): List[(ParameterKey, ParameterValue)] = {
    table.select("tr").asScala.toList
      .flatMap(tr => tr.children().asScala.toList.map(_.text()).sliding(2).map{
        case List(a, b) => (a, b)
      }.toList)
  }


  //unsafe
  def productBlock(p: Page): List[(ParameterKey, ParameterValue)]= {
    val doc = Jsoup.parse(p)
    val body = doc.body()
//    body.getElementById("bacc_45e3b058-7e9d-4f21-a042-edb11b9efdd0_372160b4-7b89-436b-80c5-d56ae52046d5_") // fronius galvo 1.5-1
    body.getElementById("bacc_04cb292b-0b0e-41ff-a44a-b3b4cc786cd9_372160b4-7b89-436b-80c5-d56ae52046d5_") // fronius galvo 2.0-1
      .getElementsByTag("table")
      .asScala.toList
      .flatMap(parseTable)
  }


  // FP println to console
  def println(line: String): IO[Unit] = IO {
    scala.Predef.println(line)
  }

  def println(seq: Seq[String]): IO[Unit] = IO {
    seq.foreach {
      l =>
        scala.Predef.println(l)
        scala.Predef.println()
    }
  }

  def println(m: Map[String, String]): IO[Unit] = IO {
    m.foreach(scala.Predef.println)
  }

  val program: IO[Unit] = for {
    httpClient <- Http1Client[IO]()
    _ <- println("Fetching Fronius Galvo 2.0-1 page")
    page <- httpClient.expect[String]("http://www.fronius.com/en/photovoltaics/products/all-products/inverters/fronius-galvo/fronius-galvo-2-0-1")
    _ <- println("GOT Fronius Galvo 2.0-1 page")
    bodyElements <- IO(productBlock(page))
    _ <- println(bodyElements.map(i => s"${i._1} -> ${i._2}"))
    _ <- println("")
    _ <- println("Closing http client")
    _ <- httpClient.shutdown
    _ <- println("See you next time, Chao!")
  } yield ()

  def main(args: Array[String]): Unit = program.unsafeRunSync()
}
