package controllers

import play.{ Logger => Log }
import play.api.mvc._
import scala.concurrent._
import _root_.models.Storage

object Photo extends Controller {
  def upload = Action {
    Ok(views.html.photo.upload.render)
  }
  def add = Action(parse.multipartFormData) { request =>
    request.body.files.foreach { tmpFile =>
      import ExecutionContext.Implicits.global
      Future {
        try {
          import java.io._
          Log.trace("Loading uploaded file: " + tmpFile)
          val ins = new BufferedInputStream(new FileInputStream(tmpFile.ref.file))
          Log.trace("Created InputStream")
          val file = Storage.file("user", tmpFile.filename)
          Log.trace("Start write to Storage")
          val stored = file.write(ins)
          Log.debug("Stored (" + stored + ") file: " + tmpFile)
          models.Photo.addNew(file.path, "This is sample")
        } catch {
          case ex: Throwable => Log.error("Failed to add new photo", ex); throw ex
        }
      }
    }
    Redirect(routes.Photo.list)
  }
  def show(id: Long) = Action {
    Ok(<ok/>)
  }
  def list = Action {
    val photos = for {
      (id, path, desc) <- models.Photo.all { p =>
        (p.id, p.path, p.desc)
      }
    } yield {
      val url = models.Storage.file(path).generateURL(60 * 60)
      (id, url, desc)
    }
    Ok(views.html.photo.list.render(photos))
  }
}
