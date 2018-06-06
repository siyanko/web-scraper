import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import scala.util._

import scala.collection.JavaConverters._

object htmlPageParser {

  type Page = String

  final case class InverterRawParameter(key: String, value: String)

  trait InverterPage {
    def parse(p: Page): Either[Throwable, List[InverterRawParameter]]
  }

  object InverterPage {
    val froniusInverterPage = new InverterPage {

      private val parseTable: Element => List[InverterRawParameter] = table => {
        table.select("tr").asScala.toList
          .flatMap(tr => tr.children().asScala.toList.map(_.text()).sliding(2).map {
            case List(a, b) => InverterRawParameter(a, b)
          }.toList)
      }

      override def parse(p: Page): Either[Throwable, List[InverterRawParameter]] = Try {
        Jsoup.parse(p).body()
          .getElementById("372160b4-7b89-436b-80c5-d56ae52046d5")
          .getElementsByTag("table")
          .asScala.toList
          .flatMap(parseTable)
      }.toEither
      
    }
  }

}
