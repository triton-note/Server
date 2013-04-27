package controllers

import play.api.mvc._
import securesocial.core.SecureSocial

object Facebook extends Controller {
  def user(accesskey: String) = Action { implicit request =>
    import scala.concurrent._
    import scala.concurrent.duration._
    val user = Await.result(models.Facebook.User(accesskey), 1 minutes)
    import service.UserCredential.Conversions._
    securesocial.controllers.ProviderController.completeAuthentication(user, session)
  }
}
