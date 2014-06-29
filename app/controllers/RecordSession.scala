package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import play.api.Logger
import play.api.libs.functional.syntax.{functionalCanBuildApplicative, toFunctionalBuilderOps}
import play.api.libs.json.{Json, __}
import play.api.mvc.{Action, Controller}

import models.Record
import models.db.{CatchReports, FishSizes, Photos}

object RecordSession extends Controller {

  def load(ticket: String) = Action.async(parse.json((
    (__ \ "count").read[Int] and
    (__ \ "offset").readNullable[Int].map(_ getOrElse 0)
  ).tupled)) { implicit request =>
    val (count, offset) = request.body
    Logger debug f"Loading records: offset(${offset}) count(${count})"
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          val records = CatchReports.find(Map(
            CatchReports.user(Some(user))
          )).map { report =>
            val comment = report.comments.find(_.user == user).map(_.text) getOrElse ""
            val photos = Photos.find(Map(
              Photos.catchReport(Some(report))
            ))
            val fishes = photos.flatMap { photo =>
              FishSizes.find(Map(
                FishSizes.photo(Some(photo))
              ))
            }
            Record(comment,
              report.createdAt,
              Record.Location(report.location, report.geoinfo),
              photos.headOption.flatMap(_.image).map(_.file.generateURL(1 hour).toString),
              fishes.toSeq.map { fish =>
                Record.Fishes(fish.name, fish.count.toInt, fish.weight.map(Record.ValueUnit.tupled), fish.length.map(Record.ValueUnit.tupled))
              })
          }
          Ok(Json toJson records)
      }
    }
  }
}