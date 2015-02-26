import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import org.fathens.play.util.Exception.allCatch
import play.api.libs.functional.syntax._
import play.api.libs.json._

package object service {
  implicit val jsonFormatTimeUnit = {
    def parse(s: String): Option[FiniteDuration] = {
      for {
        src <- Option(s)
        Array(l, u) <- allCatch opt src.split(" +")
        length <- allCatch opt l.toLong
        unit <- allCatch opt TimeUnit.valueOf {
          val up = u.toUpperCase()
          if (up.endsWith("S")) up else up + "S"
        }
      } yield FiniteDuration(length, unit)
    }
    Format[FiniteDuration](
      Reads.verifying[String](parse(_).isDefined).map(parse(_).get),
      Writes { d =>
        Json toJson f"${d.length} ${d.unit}"
      }
    )
  }
}
