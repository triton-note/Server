import java.util.concurrent.TimeUnit

import scala.concurrent.duration._

import play.api.data.validation.ValidationError
import play.api.libs.json._

import org.fathens.play.util.Exception.allCatch

import settings.Settings

package object settings {
  implicit val jsonFormatDuration = Format[FiniteDuration](
    Reads {
      _.asOpt[String].flatMap { s =>
        for {
          Array(l, u) <- allCatch opt s.split(" +", 2)
          length <- allCatch opt l.toLong
          unit <- allCatch opt TimeUnit.valueOf {
            val up = u.toUpperCase()
            if (up.endsWith("S")) up else up + "S"
          }
        } yield FiniteDuration(length, unit)
      } match {
        case Some(d) => JsSuccess(d)
        case None    => JsError(Seq(JsPath() -> Seq(ValidationError("error.expected.finiteduration"))))
      }
    },
    Writes { d =>
      Json toJson f"${d.length} ${d.unit}"
    }
  )

  /**
   * Loading of Settings
   */
  private lazy val root = {
    val file = service.Storage.file("settings.json")
    val text = io.Source.fromInputStream(file.read, "UTF-8").mkString
    (Json parse text).as[Settings]
  }
  object AWS {
    private def get(name: String): String = System.getProperty(name, System.getenv(name))

    val accessKey = get("AWS_ACCESS_KEY_ID")
    val secretKey = get("AWS_SECRET_KEY")
    val region = get("AWS_REGION")
    val bucketName = get("AWS_S3_BUCKET_NAME")
  }
  lazy val token = root.tokenTimeout
  lazy val facebook = root.facebook
  lazy val image = root.image
  lazy val openweathermap = root.openweathermap
}
