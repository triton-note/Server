package models

import scala.concurrent.duration.FiniteDuration
import play.api.mvc.Request
import play.api.Logger

class SessionValue(val name: String, val dur: FiniteDuration) {
  def apply(value: String = null): (String, String) = {
    val v = if (value == null || value.length < 1) None else Some(value)
    val vt = db.VolatileToken.createNew(dur, v)
    (name, vt.token)
  }
  def apply(req: Request[_]): Option[db.VolatileToken] = {
    val v = req.session.get(name)
    Logger debug f"Session value of $name: $v"
    val vt = v.flatMap(db.VolatileToken get _)
    Logger debug f"Gotta token $name: $vt"
    vt
  }
}
