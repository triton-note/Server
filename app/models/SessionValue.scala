package models

import scala.concurrent.duration.FiniteDuration
import play.api.mvc.Request
import play.api.Logger

class SessionValue(val name: String, val dur: FiniteDuration) {
  def apply(value: String): (String, String) = apply(Some(value))
  def apply(value: Option[String] = None): (String, String) = {
    val vt = db.VolatileTokens.createNew(dur, value)
    (name, vt.id)
  }
  def apply(req: Request[_]): Option[db.VolatileToken] = {
    val v = req.session.get(name)
    Logger debug f"Session value of $name: $v"
    val vt = v.flatMap(db.VolatileTokens get _)
    Logger debug f"Gotta token $name: $vt"
    vt
  }
}
