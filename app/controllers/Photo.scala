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
  case class AddingPhoto(date: java.sql.Timestamp, grounds: String)
  val formAddingPhoto = Form[AddingPhoto](Forms.mapping(
    "date" -> Forms.date,
    "grounds" -> Forms.nonEmptyText) {
      (date, grounds) => AddingPhoto(new java.sql.Timestamp(date.getTime), grounds)
    } {
      AddingPhoto.unapply _
    })
  def add = SecuredAction(false, None, parse.multipartFormData) { implicit request =>
    val user = request.user.user
    formAddingPhoto.bindFromRequest.fold(
      error => {
        BadRequest("NG")
      },
      adding => db.DB withTransaction {
        val album = db.Album.addNew(Some(adding.date), Some(adding.grounds))
        request.body.files.foreach { tmpFile =>
          val file = {
            val ins = {
              import java.io._
              new BufferedInputStream(new FileInputStream(tmpFile.ref.file))
            }
            val file = {
              val filename = "%d %s".format(db.DB.now.getTime, tmpFile.filename)
              models.Storage.file("photo", user.fullName, filename)
            }
            val stored = file.write(ins)
            Logger.debug("Stored (%s) file: %s".format(stored, file))
            file
          }
          val photo = db.Photo.addNew(file.path) bindTo user bindTo album
          Logger.info("Saved photo: %s".format(photo))
        }
        Redirect(routes.Photo.list)
      })
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
