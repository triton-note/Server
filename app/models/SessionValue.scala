package models

import scala.concurrent.duration.FiniteDuration
import play.api.mvc.Session

class SessionValue(val name: String, val dur: FiniteDuration) {
  def apply(value: String = null): (String, String) = {
    val v = if (value == null || value.length < 1) None else Some(value)
    val vt = db.VolatileToken.createNew(dur, v)
    (name, vt.token)
  }
  def apply(session: Session) = {
    session.get(name).flatMap { token => db.VolatileToken.get(token, db.VolatileTokenUses.Application) }
  }
}
