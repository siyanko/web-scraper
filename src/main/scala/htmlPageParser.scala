import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import scala.collection.JavaConverters._

object htmlPageParser {

  type Page = String

  final case class InverterRawParameter(key: String, value: String)

  trait InverterPage{
    def parse(p: Page): List[InverterRawParameter]
  }

  object InverterPage {
    val froniusInverterPage = new InverterPage {

      //unsafe
      private val parseTable:  Element => List[InverterRawParameter] = table => {
        table.select("tr").asScala.toList
          .flatMap(tr => tr.children().asScala.toList.map(_.text()).sliding(2).map{
            case List(a, b) => InverterRawParameter(a, b)
          }.toList)
      }

      //unsafe
      override def parse(p: Page): List[InverterRawParameter] = {
        val doc = Jsoup.parse(p)
        val body = doc.body()
        //    body.getElementById("bacc_45e3b058-7e9d-4f21-a042-edb11b9efdd0_372160b4-7b89-436b-80c5-d56ae52046d5_") // fronius galvo 1.5-1
        body.getElementById("bacc_04cb292b-0b0e-41ff-a44a-b3b4cc786cd9_372160b4-7b89-436b-80c5-d56ae52046d5_") // fronius galvo 2.0-1
          .getElementsByTag("table")
          .asScala.toList
          .flatMap(parseTable)
      }
    }
  }

}
