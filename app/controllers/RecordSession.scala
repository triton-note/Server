package controllers

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller }

import models.Report
import models.db.{ CatchReports, FishSizes, Photos }

object RecordSession extends Controller {

  def load(ticket: String) = Action.async(parse.json((
    (__ \ "count").read[Int] and
    (__ \ "last").readNullable[Long]
  ).tupled)) { implicit request =>
    val (count, last) = request.body
    Logger debug f"Loading reports: count(${count}) from ${last}"
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          val reports = CatchReports.find(
            _.withIndexName("USER-TIMESTAMP-index").withKeyConditions(Map(
                CatchReports.user compare Some(user)
              )).withScanIndexForward(false).withLimit(count).withExclusiveStartKey(
              last match {
                case Some(id) => Map(CatchReports id id)
                case None     => null
              })
          ).map { report =>
              val comment = report.comments.find(_.user == user).map(_.text) getOrElse ""
              val photos = Photos.find(
                _.withIndexName("CATCH_REPORT-index").withKeyConditions(Map(
                  Photos.catchReport compare Some(report)
                )))
              val fishes = photos.flatMap { photo =>
                FishSizes.find(
                  _.withIndexName("PHOTO-CREATED_AT-index").withKeyConditions(Map(
                    FishSizes.photo compare Some(photo)
                  )))
              }
              Report(Some(report.id),
                comment,
                report.createdAt,
                Report.Location(report.location, report.geoinfo),
                photos.headOption.flatMap(_.image).map(_.url.toString),
                fishes.toSeq.map { fish =>
                  Report.Fishes(fish.name, fish.count.toInt, fish.weight.map(Report.ValueUnit.tupled), fish.length.map(Report.ValueUnit.tupled))
                })
            }
          Ok(Json toJson reports)
      }
    }
  }
}