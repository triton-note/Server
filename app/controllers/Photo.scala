package controllers

import play.{ Logger => Log }
import play.api.mvc._
import scala.concurrent._
import models._

object Photo extends Controller {
  def upload = Action {
    Ok(views.html.photo.upload.render)
  }
  def add = Action(parse.multipartFormData) { request =>
    request.body.files.foreach { tmpFile =>
      try {
        import java.io._
        Log.trace("Loading uploaded file: " + tmpFile)
        val ins = new BufferedInputStream(new FileInputStream(tmpFile.ref.file))
        Log.trace("Created InputStream")
        val file = models.Storage.file("user", tmpFile.filename)
        Log.trace("Start write to Storage")
        val stored = file.write(ins)
        Log.debug("Stored (" + stored + ") file: " + tmpFile)
        db.Photo.addNew(file.path, "This is sample")
      } catch {
        case ex: Throwable => Log.error("Failed to add new photo", ex); throw ex
      }
    }
    Redirect(routes.Photo.list)
  }
  def show(id: Long) = Action {
    db.Photo.getById(id) match {
      case Some(photo) => {
        Ok(views.html.photo.show.render(photo.url(), photo.desc, photo.timestamp, photo.geoinfo))
      }
      case None => Ok("Not found: " + id)
    }
    
  }
  def list = Action {
    val photos = for {
      photo <- db.Photo.findByOwner(null)
    } yield {
      (photo.id, photo.url, photo.desc)
    }
    Ok(views.html.photo.list.render(photos))
  }
}
