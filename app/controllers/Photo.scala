package controllers

import play.Logger
import play.api.mvc._
import scala.concurrent._
import models._
import service.UserCredential.Conversions._

object Photo extends Controller with securesocial.core.SecureSocial {
  def upload = SecuredAction { implicit request =>
    Ok(views.html.photo.upload.render)
  }
  def add = SecuredAction(false, None, parse.multipartFormData) { implicit request =>
    request.body.files.foreach { tmpFile =>
      try {
        import java.io._
        Logger.trace("Loading uploaded file: " + tmpFile)
        val ins = new BufferedInputStream(new FileInputStream(tmpFile.ref.file))
        Logger.trace("Created InputStream")
        val file = models.Storage.file("user", tmpFile.filename)
        Logger.trace("Start write to Storage")
        val stored = file.write(ins)
        Logger.debug("Stored (" + stored + ") file: " + tmpFile)
        val photo = db.Photo.addNew(file.path, "This is sample")
        db.PhotoOwner.addNew(photo, request.user.user)
      } catch {
        case ex: Throwable => Logger.error("Failed to add new photo", ex); throw ex
      }
    }
    Redirect(routes.Photo.list)
  }
  def show(index: Long) = SecuredAction { implicit request =>
    db.Photo.getById(index) match {
      case Some(photo) => {
        Ok(views.html.photo.show.render(photo.url(), photo.desc, photo.timestamp, photo.geoinfo))
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
      (photo.id, photo.url, photo.desc)
    }
    Ok(views.html.photo.list.render(photos))
  }
}
