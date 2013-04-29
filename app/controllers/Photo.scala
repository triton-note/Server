package controllers

import play.Logger
import play.api.data._
import play.api.mvc._
import scala.concurrent._
import models._
import service.UserCredential.Conversions._

object Photo extends Controller with securesocial.core.SecureSocial {
  /**
   * Show upload form page
   */
  def upload = SecuredAction { implicit request =>
    Ok(views.html.photo.upload.render)
  }
  /**
   * Add photo by multipartForm
   */
  case class AddingPhoto(date: java.sql.Timestamp, grounds: String, comment: String)
  val formAddingPhoto = Form[AddingPhoto](Forms.mapping(
    "date" -> Forms.date,
    "grounds" -> Forms.nonEmptyText,
    "comment" -> Forms.text) {
      import db._
      (date, grounds, comment) => AddingPhoto(date, grounds, comment)
    } {
      AddingPhoto.unapply _
    })
  def add = SecuredAction(false, None, parse.multipartFormData) { implicit request =>
    implicit val user = request.user.user
    formAddingPhoto.bindFromRequest.fold(
      error => {
        BadRequest("NG")
      },
      adding => db withTransaction {
        val album = db.Album.addNew(Some(adding.date), Some(adding.grounds))
        request.body.files.foreach { tmpFile =>
          val file = {
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
          val photo = db.Photo.addNew(file.path) bindTo user bindTo album add adding.comment
          Logger.info("Saved photo: %s".format(photo))
        }
        Redirect(routes.Photo.list)
      })
  }
  def initializePhoto = SecuredAction { implicit request =>
    //
    // TODO Load form and some initialization of Photo
    //
    implicit val user = request.user.user
    db.UserAlias.list(user, db.UserAliasDomain.facebook) map { fbUser =>
      for {
        ck <- request.cookies.get(Facebook.cookieName)
        token <- db.VolatileToken.get(ck.value, db.VolatileTokenUses.Application)
        key <- token.extra
      } yield key
    }
    Ok
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
