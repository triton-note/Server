package controllers

import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{ Action, Controller }

import models.Report

object ModelView extends Controller {
  lazy val appId = settings.facebook.appId
  lazy val appName = settings.facebook.appName
  lazy val actionName = settings.facebook.publish.actionName
  lazy val objectName = settings.facebook.publish.objectName

  def catchReport(id: String) = Action.async { implicit request =>
    def qs(key: String) = request.queryString.get(key).toSeq.flatten.headOption
    Future {
      val ok = for {
        report <- Report.get(id)
      } yield {
        val title = f"Catches at ${report.fishes.headOption.map(_.name).mkString}"
        val imageUrls = report.photo.map(_.original).map(_.file generateURL settings.image.urlTimeout).map(_.toString)
        val props = Map(
          "fb:app_id" -> appId,
          "og:type" -> f"${appName}:${objectName}",
          "og:url" -> routes.ModelView.catchReport(id).absoluteURL(true),
          "og:title" -> title,
          "og:image" -> imageUrls.head,
          "og:description" -> report.fishes.map { fish =>
            val size = List(fish.length, fish.weight).flatten match {
              case Nil => ""
              case list  => list.mkString("(", ", ", ")")
            }
            f"${fish.name}${size} x ${fish.count}"
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
          "og:title" -> report.location.name,
          "place:location:latitude" -> f"${report.location.geoinfo.latitude.toDouble}%3.10f",
          "place:location:longitude" -> f"${report.location.geoinfo.longitude.toDouble}%3.10f"
        )
        Ok(views.html.spot(report.location.name, report.location.geoinfo, props))
      }
      ok getOrElse BadRequest
    }
  }
}