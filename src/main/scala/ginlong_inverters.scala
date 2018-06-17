import cats.effect._
import cats.implicits._
import console._
import htmlPageParser.{HtmlPageParser, InverterRawParameter, Page, ParsedPage}
import org.http4s.client.Client
import org.http4s.{Cookie, Headers, Request, Response, Uri}
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import scala.collection.JavaConverters._
import scala.util.Try


object ginlong_inverters {

  private val normilizeCookie: Cookie => Cookie = c => c.copy(c.name, c.content)

  private def updateCookies(init: List[Cookie], newCookies: List[Cookie]): List[Cookie] =
    init ++ newCookies.map(normilizeCookie)

  private def updateCookies(init: List[Cookie], c: Cookie): List[Cookie] = c :: init

  private val toHeaderCookie: Cookie => org.http4s.headers.Cookie = c => org.http4s.headers.Cookie(c)

  private val ginlongInvertersListParser: HtmlPageParser[Page, List[InverterRequestPage]] = new HtmlPageParser[Page, List[InverterRequestPage]] {

    val parseInverterItems: Element => List[InverterRequestPage] = el => {
      val duplicates = for {
        div <- el.getAllElements.asScala.toList
        a <- div.getElementsByTag("a").asScala.filter(_.hasClass("pro_a")).headOption.toList
        h2 <- a.getElementsByTag("h2").asScala.headOption.toList
      } yield InverterRequestPage(a.attr("href"), h2.text())

      duplicates.toSet.toList
    }

    override def parse(p: Page): ParsedPage[List[InverterRequestPage]] = Try {
      Jsoup.parse(p)
        .getAllElements.asScala.toList
        .find(_.hasClass("main_right")).toList
        .flatMap(_.getAllElements.asScala.toList)
        .find(_.hasClass("row")).toList
        .flatMap(parseInverterItems)
    }.toEither
  }

  private val inverterPageParser: HtmlPageParser[Page, List[InverterRawParameter]] = new HtmlPageParser[Page, List[InverterRawParameter]] {

    private val parseTable: Element => List[InverterRawParameter] =
      _.getElementsByTag("tr").asScala.toList
        .flatMap(_.getElementsByTag("td").asScala.toList)
        .map(_.text()).sliding(2).toList.map {
        case List(a, b) => InverterRawParameter(a, b)
      }

    val parseDetailBox: Element => List[InverterRawParameter] =
      _.getElementsByTag("table").asScala.toList
        .flatMap(parseTable)

    override def parse(p: Page): ParsedPage[List[InverterRawParameter]] = Try {
      Jsoup.parse(p)
        .getAllElements.asScala.toList
        .find(_.hasClass("detail_box")) match {
        case Some(el) => parseDetailBox(el)
        case None => List.empty[InverterRawParameter]
      }
    }.toEither
  }

  private val parseUri: String => Option[Uri] = s => Uri.fromString(s) match {
    case Left(_) => None
    case Right(uri) => Some(uri)
  }

  def httpFetch[F[_] : Effect, A](uri: Uri,
                                  cookies: List[Cookie])
                                 (f: Response[F] => A): Client[F] => F[A] = _.fetch[A](
    Request[F](
      uri = uri,
      headers = Headers(
        cookies.map(toHeaderCookie)
      )))(r => Effect[F].point(f(r)))

  def httpFetch2[F[_], A](httpClient: Client[F], uri: Uri, cookies: List[Cookie], f: Response[F] => F[A]): F[A] =
    httpClient.fetch[A](
      Request[F](
        uri = uri,
        headers = Headers(
          cookies.map(toHeaderCookie)
        )
      )
    )(f)

  def getRequestCookies[F[_] : Effect : Console](httpClient: Client[F]): F[List[Cookie]] = for {
    _ <- Console[F].println("Fetching yunsuoSessionVerifyCookie")
    yunsuoSessionVerifyCookie <- httpFetch2[F, List[Cookie]](
      httpClient,
      Uri.uri("http://www.ginlong.com/en/PV_Inverters.html?security_verify_data=313434302c393030"),
      Nil,
      resp => Effect[F].point(resp.cookies)
    )

    cookies1 = updateCookies(yunsuoSessionVerifyCookie,
      Cookie("srcurl", "687474703a2f2f7777772e67696e6c6f6e672e636f6d2f656e2f50565f496e766572746572732e68746d6c")
    )
    _ <- Console[F].println("Fetching securitySessionMidVerify")
    securitySessionMidVerify <- httpFetch2[F, List[Cookie]](
      httpClient,
      Uri.uri("http://www.ginlong.com/en/PV_Inverters.html?security_verify_data=313434302c393030"),
      cookies1,
      resp => Effect[F].point(resp.cookies)
    )

    cookies2 = updateCookies(cookies1, securitySessionMidVerify)

  } yield cookies2

  def getPageLoop[F[_] : Effect : Console](httpClient: Client[F], uri: Uri, cookies: List[Cookie]): F[Either[Throwable, List[InverterRawParameter]]] = {

    def loop(uri: Uri): F[Either[Throwable, List[InverterRawParameter]]] = for {
      maybePage <- httpFetch2[F, Either[Throwable, String]](
        httpClient, uri, cookies, _.bodyAsText.compile.foldMonoid.attempt
      )

      result <- maybePage match {
        case Left(err) => Effect[F].point(Left(err))
        case Right(page) =>
          ginlongInvertersListParser.parse(page) match {
            case Left(_) => Effect[F].point(inverterPageParser.parse(page))
            case Right(ls) if ls.nonEmpty =>
              ls.map(p => p.copy(url = "http://www.ginlong.com" + p.url + "?l=en")) match {
                case p :: _ => {
                  parseUri(p.url) match {
                    case Some(uri) => loop(uri)
                    case None => Effect[F].point(Right(List.empty[InverterRawParameter]))
                  }
                }
                case _ => Effect[F].point(Right(List.empty[InverterRawParameter]))
              }
            case Right(ls) if ls.isEmpty => Effect[F].point(inverterPageParser.parse(page))
          }
      }
    } yield result


    loop(uri)
  }

  def ginlongInverters[F[_] : Effect : Console]: Client[F] => F[Unit] = httpClient => for {
    cookies <- getRequestCookies[F](httpClient)
    _ <- Console[F].println("Fetching First Inverter Details")
    inverterDetails <- getPageLoop(httpClient, Uri.uri("http://www.ginlong.com/en/PV_Inverters.html?l=en"), cookies)
    _ <- inverterDetails match {
      case Left(err) => for {
        _ <- Console[F].println("Got ERROR")
        _ <- Console[F].println(err.getStackTrace.map(_.toString).toList)
      } yield ()

      case Right(details) => Console[F].println(details.map(_.toString))
    }
  } yield ()

}
