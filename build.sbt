name := "web-scraper"

version := "0.1"

scalaVersion := "2.12.6"

val http4sVersion = "0.18.12"

val circeVersion = "0.9.3"

libraryDependencies ++= Seq(
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-server" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.http4s" %% "http4s-circe" % http4sVersion,

  //html parsing... jsoup
  "org.jsoup" % "jsoup" % "1.11.3"
)

scalacOptions ++= Seq("-Ypartial-unification")