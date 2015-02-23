package controllers

import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{ Action, Controller }

import models.Report
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
        report <- Report.get(id)
      } yield {
        val title = f"Catches at ${report.fishes.headOption.map(_.nominal).mkString}"
        val imageUrls = report.photo.map(_.original).map(_ generateURL Settings.Image.urlExpiration).map(_.toString)
        val props = Map(
          "fb:app_id" -> appId,
          "og:type" -> f"${appName}:${objectName}",
          "og:url" -> routes.ModelView.catchReport(id).absoluteURL(true),
          "og:title" -> title,
          "og:image" -> imageUrls.head,
          "og:description" -> report.fishes.map { fish =>
            val size = List(fish.sizeLength, fish.sizeWeight).flatten match {
              case Nil => ""
              case list  => list.mkString("(", ", ", ")")
            }
            f"${fish.nominal}${size} x ${fish.quantity}"
          }.mkString("\n")
        )
        Ok(views.html.catchReport(title, report.fishes, imageUrls, props))
      }
      ok getOrElse BadRequest
    }
  }
  def spot(id: String) = Action.async { implicit request =>
    Future {
      val ok = for {
        report <- Report.get(id)
      } yield {
        val props = Map(
          "fb:app_id" -> appId,
          "og:type" -> "place",
          "og:url" -> routes.ModelView.spot(id).absoluteURL(true),
          "og:title" -> report.location.nominal,
          "place:location:latitude" -> f"${report.location.geoinfo.latitude.toDouble}%3.10f",
          "place:location:longitude" -> f"${report.location.geoinfo.longitude.toDouble}%3.10f"
        )
        Ok(views.html.spot(report.location.nominal, report.location.geoinfo, props))
      }
      ok getOrElse BadRequest
    }
  }
}