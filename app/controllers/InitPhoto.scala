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
  private def completeAuth(ses: (String, String)*)(implicit user: db.UserAlias) = {
    securesocial.core.Authenticator.create(user) match {
      case Left(error) => throw error
      case Right(authenticator) => {
        val cookies = List(authenticator.toCookie).map(_.copy(secure = true))
        val extra = <header>
                      {
                        for {
                          c <- cookies
                        } yield <cookie name={ c.name } value={ c.value } maxAge={ c.maxAge.map(_.toString) orNull }/>
                      }
                      {
                        for {
                          (name, value) <- ses
                        } yield <session name={ name } value={ value }/>
                      }
                    </header>
        Logger debug f"Saving cookies and session as XML: $extra"
        val vt = db.VolatileToken.createNew(5 minutes, Some(extra.toString))
        Ok(vt.token).withCookies(cookies: _*).withSession(ses: _*)
      }
    }
  }
  /**
   * Login as Facebook user
   */
  def createTokenByFacebook = Action.async(parse.text) { implicit request =>
    val accesskey = request.body
    for {
      u <- Facebook.User(accesskey)
    } yield {
      Logger debug f"Authorized user from facebook: $u"
      u map { implicit user =>
        completeAuth(
          sessionFacebook(accesskey),
          sessionUploading()
        )
      } getOrElse Results.Unauthorized
    }
  }
  /**
   * Save uploaded xml of file info
   */
  def saveInfo = SecuredAction(parse.xml) { implicit request =>
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
  /**
   * Form of initializing photo
   */
  case class InitInput(filepath: String, date: java.util.Date, grounds: String, comment: String)
  val formInitInput = Form[InitInput](
    Forms.mapping(
      "filepath" -> Forms.nonEmptyText,
      "date" -> Forms.date("yyyy-MM-dd"),
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
  def cancel = SecuredAction(parse.xml) { implicit request =>
    implicit val user = request.user.user
    val uploading = sessionUploading(request)
    import models.RichXML._
    val filepath = request.body.head \@ "filepath"
    Logger.debug(f"Canceling $filepath in $uploading")
    val ok = uploading.flatMap { vt =>
      vt.extra.toList.flatMap(PreInfo.read).find(_.basic.filepath == filepath).map { info =>
        Ok("Canceled")
      }
    }
    ok getOrElse Results.BadRequest
  }
  /**
   * Uploaded photo data
   */
  def upload = SecuredAction(parse.raw) { implicit request =>
    implicit val user = request.user.user
    val uploading = sessionUploading(request)
    Logger.debug(f"Uploading photo with $uploading")
    val ok = uploading.flatMap { vt =>
      val tmp = request.body.asFile
        Logger.trace(f"Uploaded $tmp")
      // 現在のところファイルは１つずつしかアップロード出来ない
      vt.extra.flatMap(xml => (PreInfo read xml).headOption).map(_.basic.filepath).map { filename =>
        InferencePreInfo.commitByUpload(vt)(filename, tmp).map { a =>
          for {
            committed <- a
            v <- sessionFacebook(request)
            accessKey <- v.extra
          } yield PublishPhotoCollection.add(committed)(Facebook.AccessKey(accessKey), 3 minutes)
        }
      // Ignore result on Future
      Ok("OK")
    }
    }
    ok getOrElse Results.BadRequest
  }
}
