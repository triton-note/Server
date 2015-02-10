package controllers

import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{ Action, Controller }

import models.db.{ CatchReport, FishSize, Photo }
import service.Settings

object ModelView extends Controller {
  val appId = Settings.FACEBOOK_APP_ID
  val appName = Settings.FACEBOOK_APP_NAME
  val actionName = Settings.FACEBOOK_CATCH_ACTION
  val objectName = Settings.FACEBOOK_CATCH_OBJECT

  def catchReport(id: String) = Action.async { implicit request =>
    def qs(key: String) = request.queryString.get(key).toSeq.flatten.headOption
    Future {
      val ok = for {
        report <- CatchReport.get(id)
      } yield {
        val (images, fishes) = Photo.findBy(report).map { photo =>
          (photo.image, FishSize findBy photo)
        }.unzip match {
          case (list1, list2) => (list1.flatten, list2.flatten)
        }
        val title = f"Catches at ${report.timestamp}"
        val imageUrls = images.map(_ url Settings.Image.urlExpiration).map(_.toString)
        val props = Map(
          "fb:app_id" -> appId,
          "og:type" -> f"${appName}:${objectName}",
          "og:url" -> routes.ModelView.catchReport(id).absoluteURL(request.secure),
          "og:title" -> title,
          "og:image" -> imageUrls.head,
          "og:description" -> fishes.map { fish =>
            val size = fish.size.toString match {
              case "" => ""
              case s  => f"(${s})"
            }
            f"${fish.name}${size} x ${fish.count}"
          }.mkString("\n")
        )
        Ok(views.html.catchReport(title, fishes, imageUrls, props))
      }
      ok getOrElse BadRequest
    }
  }
  def spot(id: String) = Action.async { implicit request =>
    Future {
      val ok = for {
        report <- CatchReport.get(id)
      } yield {
        val props = Map(
          "fb:app_id" -> appId,
          "og:type" -> "place",
          "og:url" -> routes.ModelView.spot(id).absoluteURL(request.secure),
          "og:title" -> report.location,
          "place:location:latitude" -> report.geoinfo.latitude.toString,
          "place:location:longitude" -> report.geoinfo.longitude.toString
        )
        Ok(views.html.spot(report.location, report.geoinfo, props))
      }
      ok getOrElse BadRequest
    }
  }
}