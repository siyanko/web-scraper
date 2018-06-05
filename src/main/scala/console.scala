import cats.effect.IO

object console {

  trait Console[F[_]] {
    def println(line: String): F[Unit]

    def println(): F[Unit]

    def println(ls: List[String]): F[Unit]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F

    implicit val ConsoleIO = new Console[IO] {
      override def println(line: String): IO[Unit] = IO(scala.Predef.println(line))

      override def println(): IO[Unit] = IO(scala.Predef.println())

      override def println(ls: List[String]): IO[Unit] = IO {
        ls.foreach(scala.Predef.println)
      }
    }
  }

}
