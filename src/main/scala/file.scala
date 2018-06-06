import scala.util._
import java.io._
import cats.effect.IO



object file {
    trait FileStorage[F[_]]{
        def save(fileName: String, data: String): F[Either[Throwable, Unit]]
    }

    object FileStorage {
        def apply[F[_]](implicit F: FileStorage[F]): FileStorage[F] = F

        implicit val FileStorageIO: FileStorage[IO] = new FileStorage[IO]{
            override def save(fileName: String, data: String): IO[Either[Throwable, Unit]] = IO{  
                Try {             
                    val writer: BufferedWriter = new BufferedWriter(new FileWriter(fileName))
                    writer.write(data)
                    writer.close()
                }.toEither
            }
        }
    }
}