package controllers

import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import play.api.Logger
import play.api.data._
import play.api.mvc._
import play.api.libs.Files._
import models._
import models.UncommittedPhoto._
import service.UserCredential.Conversions._
import service.PublishPhotoCollection

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
    } getOrElse Status(401) // Unauthorized
  }
  /**
   * Save uploaded xml of file info
   */
  def saveInfo = SecuredAction(false, None, parse.xml) { implicit request =>
    val ok = for {
      vt <- sessionUploading(request)
      xml <- request.body.headOption
      infos <- PreInfo load xml
    } yield {
      vt setExtra PreInfo.asXML(infos)
      inference(vt)
      Ok("OK")
    }
    ok getOrElse BadRequest("No session")
  }
  /**
   * Getting info of photos in session for JavaScript
   */
  def getInfos = SecuredAction { implicit request =>
    val ok = for {
      vt <- sessionUploading(request)
      infos = PreInfo load vt
    } yield Ok(PreInfo asXML infos)
    ok getOrElse BadRequest("No session")
  }
  /**
   * Show page of form for initializing photo
   */
  def showForm = SecuredAction { implicit request =>
    val ok = for {
      vt <- sessionUploading(request)
      infos = PreInfo load vt
    } yield Ok(views.html.photo.init render infos)
    ok getOrElse BadRequest("No session")
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
   * Set initializing info by user
   */
  def submit = SecuredAction { implicit request =>
    implicit val user = request.user.user
    formInitInput.bindFromRequest.fold(
      error => {
        BadRequest("Mulformed parameters")
      },
      adding => db.withTransaction {
        val ok = for {
          vt <- sessionUploading(request)
          info <- (PreInfo load vt).find(_.basic.filepath == adding.filepath)
        } yield {
          val next = info.update(adding.date, adding.grounds, adding.comment)
          update(vt, next)
          Ok("Updated")
        }
        ok getOrElse BadRequest("NG")
      }
    )
  }
  /**
   * Uploaded photo data
   */
  def upload = SecuredAction(false, None, parse.multipartFormData) { implicit request =>
    implicit val user = request.user.user
    db.withTransaction {
      def commit(list: List[PreInfo]) = for {
        file <- request.body.files
        info <- list.find(_.basic.filepath == file.filename)
        committed <- info.commit(file.ref.file)
      } yield committed
      val ok = for {
        vt <- sessionUploading(request)
      } yield {
        val committed = commit(PreInfo load vt)
        if (committed.nonEmpty) {
          val allInfos = update(vt, committed: _*)
          for {
            v <- sessionFacebook(request)
            accessKey <- v.extra
          } yield PublishPhotoCollection.add(allInfos)(Facebook.AccessKey(accessKey), 10 minutes)
          Ok("OK")
        } else BadRequest("NG")
      }
      ok getOrElse BadRequest("NG")
    }
  }
}
