import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import org.fathens.play.util.Exception.allCatch
import play.api.libs.functional.syntax._
import play.api.libs.json._

package object service {
  implicit val jsonFormatTimeUnit = Format[FiniteDuration](
    (
      (__ \ "length").read[Long] and
      (__ \ "unit").read[String](Reads.verifying[String] { s =>
        (allCatch opt TimeUnit.valueOf(s)).isDefined
      }).map(TimeUnit.valueOf)
    )((l, u) => FiniteDuration(l, u)),
    Writes { d =>
      Json.obj(
        "length" -> d.length,
        "unit" -> d.unit.toString
      )
    }
  )
}
