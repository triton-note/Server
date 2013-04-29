package controllers

import play.api.mvc._
import securesocial.core.SecureSocial

object Facebook extends Controller {
  val cookieName = "facebook_accesskey"
  def user(accesskey: String) = Action { implicit request =>
    import scala.concurrent._
    import scala.concurrent.duration._
    val r = Await.result(models.Facebook.User(accesskey), 1 minutes) map { user =>
      val vt = models.db.VolatileToken.createNew(1 hour, Some(accesskey))
      import service.UserCredential.Conversions._
      val res = securesocial.controllers.ProviderController.completeAuthentication(user, session)
      res.withCookies(new Cookie(cookieName, vt.token))
    }
    r getOrElse Ok
  }
}
