import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import scala.util._

import scala.collection.JavaConverters._

object htmlPageParser {

  type ParsedPage[A] = Either[Throwable, A]

  type Page = String

  final case class InverterRawParameter(key: String, value: String)

  trait HtmlPageParser[A, B] {
    def parse(p: A): ParsedPage[B]
  }

  object HtmlPageParser {

    def parse[A, B](page: A)(implicit P: HtmlPageParser[A, B]): ParsedPage[B] = P.parse(page)

    implicit val froniusHtmlPageParser: HtmlPageParser[Page, List[InverterRawParameter]] = new HtmlPageParser[Page, List[InverterRawParameter]] {

      private val parseTable: Element => List[InverterRawParameter] = _.select("tr").asScala.toList
        .flatMap(tr => tr.children().asScala.toList.map(_.text()).sliding(2).map {
          case List(a, b) => InverterRawParameter(a, b)
        }.toList)


      override def parse(p: Page): ParsedPage[List[InverterRawParameter]] = Try {
        Jsoup.parse(p)
          .body()
          .getElementById("372160b4-7b89-436b-80c5-d56ae52046d5")
          .getElementsByTag("table")
          .asScala.toList
          .flatMap(parseTable)
      }.toEither

    }

    implicit val froniusInvertersListParser: HtmlPageParser[Page, List[InverterRequestPage]] = new HtmlPageParser[Page, List[InverterRequestPage]] {

      private val parseArticle: Element => List[InverterRequestPage] = article => for {
        tag <- article.getElementsByClass("tags").asScala.toList
        a <- tag.getElementsByTag("a").asScala.toList
      } yield InverterRequestPage(a.attr("href"), a.text())


      override def parse(p: Page): ParsedPage[List[InverterRequestPage]] = Try {
        Jsoup.parse(p)
          .body()
          .getElementById("3495-inverters")
          .getElementsByTag("article")
          .asScala.toList
          .flatMap(parseArticle)
      }.toEither
    }
  }

}
