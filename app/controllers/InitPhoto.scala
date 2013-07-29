package controllers

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.control.Exception._
import ExecutionContext.Implicits.global
import play.api.Logger
import play.api.data._
import play.api.mvc._
import models._
import service._
import service.UserCredential.Conversions._

object InitPhoto extends Controller with securesocial.core.SecureSocial {
  val sessionFacebook = new SessionValue("TritonNote-facebook_accesskey", 30 minutes)
  val sessionUploading = new SessionValue("TritonNote-uploading", 1 hour)
  /**
   * Register authorized user to session
   */
  private def completeAuth(ses: (String, String)*)(implicit user: db.UserAlias): PlainResult = {
    securesocial.core.Authenticator.create(user) match {
      case Left(error) => throw error
      case Right(authenticator) => {
        val cookies = List(authenticator.toCookie).map(_.copy(secure = true))
        val extra = <header>
                      {
                        for {
                          c <- cookies
                        } yield <cookie name={ c.name } value={ c.value } maxAge={ c.maxAge.map(_.toString) orNull }/>
                        for {
                          (name, value) <- ses
                        } yield <session name={ name } value={ value }/>
                      }
                    </header>
        val vt = db.VolatileToken.createNew(5 minutes, Some(extra.toString));
        Ok(vt.token).withCookies(cookies: _*).withSession(ses: _*)
      }
    }
  }
  /**
   * Login as Facebook user
   */
  def createTokenByFacebook = Action(parse.text) { implicit request =>
    Async {
      val accesskey = request.body
      for {
        u <- Facebook.User(accesskey)
      } yield {
        Logger debug f"Authorized user from facebook: $u"
        u flatMap { implicit user =>
          allCatch opt completeAuth(
            sessionFacebook(accesskey),
            sessionUploading()
          )
        } getOrElse Results.Unauthorized
      }
    }
  }
  /**
   * Save uploaded xml of file info
   */
  def saveInfo = SecuredAction(false, None, parse.xml) { implicit request =>
    val ok = for {
      vt <- sessionUploading(request)
      xml <- request.body.headOption
    } yield {
      val infos = InferencePreInfo.initialize(vt, xml)
      // Ignore result on Future
      Ok("OK")
    }
    ok getOrElse Results.BadRequest
  }
  /**
   * Getting info of photos in session for JavaScript
   */
  def getInfo = SecuredAction { implicit request =>
    val ok = for {
      vt <- sessionUploading(request)
      xml <- vt.extra
    } yield {
      val infos = PreInfo read xml
      Ok(PreInfo asXML infos)
    }
    ok getOrElse Results.BadRequest
  }
  def showForm_redirect(key: String) = Action { implicit request =>
    val ok = for {
      vt <- db.VolatileToken get key
      extra <- vt.extra
      xml <- allCatch opt (scala.xml.XML loadString extra)
    } yield {
      val cookies = for {
        c <- xml \ "cookie"
        name <- c \@ "name"
        value <- c \@ "value"
        maxAge = (c \@# "maxAge").map(_.toInt)
      } yield Cookie(name, value, maxAge, secure = true)
      val ses = for {
        s <- xml \ "session"
        name <- s \@ "name"
        value <- s \@ "value"
      } yield (name, value)
      Redirect(routes.InitPhoto.showForm).withCookies(cookies: _*).withSession(ses: _*)
    }
    ok getOrElse Results.BadRequest
  }
  /**
   * Show page of form for initializing photo
   */
  def showForm = SecuredAction { implicit request =>
    val ok = for {
      vt <- sessionUploading(request)
      xml <- vt.extra
    } yield {
      val infos = PreInfo read xml
      Ok(views.html.photo.init render infos)
    }
    ok getOrElse Results.BadRequest
  }
  /**
   * Form of initializing photo
   */
  case class InitInput(filepath: String, date: java.util.Date, grounds: String, comment: String)
  val formInitInput = Form[InitInput](
    Forms.mapping(
      "filepath" -> Forms.nonEmptyText,
      "date" -> Forms.date,
      "grounds" -> Forms.nonEmptyText,
      "comment" -> Forms.text
    )(InitInput.apply)(InitInput.unapply)
  )
  /**
   * Set initializing info by user
   */
  def submit = SecuredAction { implicit request =>
    implicit val user = request.user.user
    formInitInput.bindFromRequest.fold(
      error => {
        Results.BadRequest
      },
      adding => {
        val ok = for {
          vt <- sessionUploading(request)
        } yield {
          val info = InferencePreInfo.submitByUser(vt)(adding.filepath, adding.date, adding.grounds, adding.comment)
          // Ignore result on Future
          Ok("Updated")
        }
        ok getOrElse Results.BadRequest
      }
    )
  }
  /**
   * Uploaded photo data
   */
  def upload = SecuredAction(false, None, parse.multipartFormData) { implicit request =>
    implicit val user = request.user.user
    val ok = sessionUploading(request).map { vt =>
      request.body.files.map { tmp =>
        InferencePreInfo.commitByUpload(vt)(tmp.filename, tmp.ref.file).map { a =>
          for {
            committed <- a
            v <- sessionFacebook(request)
            accessKey <- v.extra
          } yield PublishPhotoCollection.add(committed)(Facebook.AccessKey(accessKey), 3 minutes)
        }
      }
      // Ignore result on Future
      Ok("OK")
    }
    ok getOrElse Results.BadRequest
  }
}
