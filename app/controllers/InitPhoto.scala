package controllers

import scala.concurrent._
import scala.concurrent.duration._

import play.api.Logger
import play.api.data._
import play.api.mvc._
import play.api.libs.Files._
import models._
import models.UncommittedPhoto._
import service.UserCredential.Conversions._

object InitPhoto extends Controller with securesocial.core.SecureSocial {
  val sessionFacebook = new SessionValue("TritonNote-facebook_accesskey", 1 hour)
  val sessionUploading = new SessionValue("TritonNote-uploading", 1 hour)
  /**
   * Register authorized user to session
   */
  private def completeAuth(user: db.UserAlias)(implicit request: RequestHeader): Result = {
    securesocial.controllers.ProviderController.completeAuthentication(user, request.session)
  }
  /**
   * Login as Facebook user
   */
  def createTokenByFacebook(accesskey: String) = Action { implicit request =>
    Await.result(Facebook.User(accesskey), 1 minutes) map { user =>
      completeAuth(user).withSession(
        sessionFacebook(accesskey),
        sessionUploading())
    } getOrElse Status(401)
  }
  /**
   * Save uploaded xml of file info
   */
  def saveInfo = SecuredAction(false, None, parse.xml) { implicit request =>
    val ok = for {
      vt <- sessionUploading(request.session)
      xml <- request.body.headOption
      infos <- PreInfo load xml
    } yield {
      vt setExtra PreInfo.asXML(infos)
      inference(vt)
      Ok("OK")
    }
    ok getOrElse BadRequest(<ng>No session</ng>)
  }
  /**
   * Show page of form for initializing photo
   */
  def showForm = SecuredAction { implicit request =>
    val ok = for {
      vt <- sessionUploading(request.session)
      ex <- vt.extra
      xml = scala.xml.XML loadString ex
      analyzed <- PreInfo load xml
    } yield Ok(views.html.photo.init.render(analyzed))
    ok getOrElse BadRequest(<ng>No session</ng>)
  }
  /**
   * Form of initializing photo
   */
  case class InitInput(filepath: String, date: java.sql.Timestamp, grounds: String, comment: String)
  val formInitInput = Form[InitInput](Forms.mapping(
    "filepath" -> Forms.nonEmptyText,
    "date" -> Forms.date,
    "grounds" -> Forms.nonEmptyText,
    "comment" -> Forms.text) {
      import db._
      (filepath, date, grounds, comment) => InitInput(filepath, date, grounds, comment)
    } {
      InitInput.unapply _
    })
  /**
   * Initializing photo info
   */
  def submit = SecuredAction { implicit request =>
    formInitInput.bindFromRequest.fold(
      error => {
        BadRequest("Mulformed parameters")
      },
      adding => db.withTransaction {
        val ok = for {
          vt <- sessionUploading(request.session)
          infos = PreInfo.load(vt)
          info <- infos.find(_.filepath == adding.filepath)
        } yield {
          info.committed match {
            case None => {
              val next = info.copy(grounds = Some(adding.grounds), date = Some(adding.date), comment = Some(adding.comment))
              val list = next :: infos.filter(_.filepath != adding.filepath)
              vt setExtra PreInfo.asXML(list)
              Ok("Not yet committed")
            }
            case Some(id) => db withTransaction {
              Ok("Updated")
            }
          }
        }
        ok getOrElse BadRequest("NG")
      })
  }
  def upload = SecuredAction(false, None, parse.multipartFormData) { implicit request =>
    val fb = sessionFacebook(request.session)
    val ok = for {
      vt <- sessionUploading(request.session).toList
      info <- PreInfo.load(vt)
      file <- request.body.files
      if (info.filepath == file.filename)
    } yield {
      val committed = info.commit(request.user, file.ref.file)
      for {
        v <- fb
        accessKey <- v.extra
      } yield publishToFacebook(accessKey, committed)
      Ok("OK")
    }
    ok.headOption getOrElse BadRequest("NG")
  }
  private def publishToFacebook(accessKey: String, info: PreInfo) {
    // TODO Publish to Facebook
  }
}
