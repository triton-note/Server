package controllers

import play.Logger
import play.api.data._
import play.api.mvc._
import scala.concurrent._
import models._
import service.UserCredential.Conversions._
import scala.concurrent._
import scala.concurrent.duration._

object InitPhoto extends Controller with securesocial.core.SecureSocial {
  class CookieToken(val name: String) {
    def apply(xml: scala.xml.Elem)(implicit dur: FiniteDuration = 1 hour): Cookie = apply(xml.toString)
    def apply(value: String)(implicit dur: FiniteDuration = 1 hour): Cookie = apply(Some(value))
    def apply(value: Option[String])(implicit dur: FiniteDuration = 1 hour): Cookie = {
      val vt = db.VolatileToken.createNew(dur, value)
      new Cookie(name, vt.token)
    }
  }
  val cookieFacebook = new CookieToken("TritonNote-facebook_accesskey")
  val cookieUploading = new CookieToken("TritonNote-uploading")
  /**
   * Register authorized user to session
   */
  def completeAuth(user: db.UserAlias)(implicit request: RequestHeader) = {
    import service.UserCredential.Conversions._
    securesocial.controllers.ProviderController.completeAuthentication(user, request.session)
  }
  /**
   * Login as Facebook user
   */
  def facebook(accesskey: String) = Action { implicit request =>
    Await.result(Facebook.User(accesskey), 1 minutes) map { user =>
      completeAuth(user).withCookies(
        cookieFacebook(accesskey),
        cookieUploading(None))
    } getOrElse Status(401)
  }
  /**
   * Show page of initializing photo
   */
  def formInit = SecuredAction { implicit request =>
    //
    // TODO give date and grounds by user profile
    //
    val dateList = List(db.currentTimestamp)
    val groundsList = Nil
    val cs = for {
      a <- List(cookieFacebook, cookieUploading)
      c <- request.cookies.get(a.name)
    } yield c
    Ok(views.html.photo.init.render(dateList, groundsList)).withCookies(cs: _*)
  }
  /**
   * Form of initializing photo
   */
  case class InitInput(date: java.sql.Timestamp, grounds: String, comment: String)
  val formInitInput = Form[InitInput](Forms.mapping(
    "date" -> Forms.date,
    "grounds" -> Forms.nonEmptyText,
    "comment" -> Forms.text) {
      import db._
      (date, grounds, comment) => InitInput(date, grounds, comment)
    } {
      InitInput.unapply _
    })
  /**
   * Initializing photo and waiting upload complete
   */
  def submitInit = SecuredAction { implicit request =>
    formInitInput.bindFromRequest.fold(
      error => {
        BadRequest("Mulformed parameters")
      },
      adding => {
        implicit val user = request.user.user
        for {
          c <- request.cookies.get(cookieUploading.name)
          file <- awaitUploading(user, c.value)
        } yield db withTransaction {
          val album = db.Album.addNew(Some(adding.date), Some(adding.grounds))
          val photo = db.Photo.addNew(file.path) bindTo user bindTo album add adding.comment
          Logger.info("Saved photo: %s".format(photo))
          db.UserAlias.list(user, db.UserAliasDomain.facebook) map { fbUser =>
            for {
              ck <- request.cookies.get(cookieFacebook.name)
              token <- db.VolatileToken.get(ck.value, db.VolatileTokenUses.Application)
              key <- token.extra
            } yield {
              // TODO Publish to Facebook
            }
          }
          Redirect(routes.InitPhoto.show(photo.id))
        }
      } getOrElse BadRequest("ng"))
  }
  /**
   * Wait upload complete
   */
  def awaitUploading(user: db.User, token: String): Option[Storage.S3File] = {
    val ins = {
      import java.io._
      new BufferedInputStream(new FileInputStream(tmpFile.ref.file))
    }
    val file = {
      val filename = "%d %s".format(System.currentTimeMillis, tmpFile.filename)
      Storage.file("photo", user.fullName, filename)
    }
    val stored = file.write(ins)
    Logger.debug("Stored (%s) file: %s".format(stored, file))
    file
  }
  /**
   * Show specified photo
   */
  def show(index: Long) = SecuredAction { implicit request =>
    db.Photo.getById(index) match {
      case Some(photo) => {
        Ok(views.html.photo.show.render(photo.url, photo.timestamp, photo.geoinfo))
      }
      case None => Ok("Not found: " + index)
    }

  }
  def list = SecuredAction { implicit request =>
    val id = request.user
    Logger.debug("Listing up by identity: %s".format(id))
    val photos = for {
      photo <- db.Photo.findByOwner(id.user)
    } yield {
      (photo.id, photo.url)
    }
    Ok(views.html.photo.list.render(photos))
  }
}
