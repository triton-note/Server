package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.mvc.{Action, Controller}

import models.{Facebook, SessionValue, Settings}

object Account extends Controller {
  val sessionFacebook = new SessionValue("TritonNote-facebook_accesskey", Settings.Session.timeoutFacebook)
  val sessionUploading = new SessionValue("TritonNote-uploading", Settings.Session.timeoutUpload)

  def createTokenByFacebook = Action.async(parse.multipartFormData) { implicit request =>
    def part(name: String) = request.body.dataParts(name).headOption
    val user = {
      val token = part("token").get
      part("way").get match {
        case "facebook" => Facebook.User(token)
        case _ => Future(None)
      }
    }
    for {
      u <- user
    } yield {
      Logger debug f"Authorized user from facebook: $u"
      u map { implicit user =>
        completeAuth(
          sessionFacebook(accesskey),
          sessionUploading())
      } getOrElse Results.Unauthorized
    }
  }
}
