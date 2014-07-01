package controllers

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}

import models.Report
import models.db.{CatchReports, Comments, FishSize, FishSizes, Photos}

object ReportSync extends Controller {

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

  def update(ticket: String) = Action.async(parse.json((
    (__ \ "report").read[Report]
  ))) { implicit request =>
    val report = request.body
    Logger debug f"Updating ${report}"
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          report.id.flatMap(CatchReports.get) match {
            case None => BadRequest(f"Invalid id: ${report.id.orNull}")
            case Some(src) =>
              CatchReports.update(src.id, List(
                CatchReports.timestamp.diff(src.timestamp, report.dateAt),
                CatchReports.location.diff(src.location, report.location.name),
                CatchReports.latitude.diff(src.latitude, report.location.geoinfo.latitude.toDouble),
                CatchReports.latitude.diff(src.longitude, report.location.geoinfo.longitude.toDouble)
              ).flatten.toMap) match {
                case None => InternalServerError("Failed to update report")
                case Some(doneCR) =>
                  doneCR.comments.find(_.user == user).headOption.flatMap { comment =>
                    Comments.update(comment.id, List(
                      Comments.text.diff(comment.text, report.comment)
                    ).flatten.toMap)
                  }
                  Photos.findByCatchReport(doneCR) match {
                    case thePhoto :: Nil =>
                      def reduce(east: List[FishSize], west: List[Report.Fishes], left: List[FishSize] = Nil): (List[FishSize], List[Report.Fishes]) = west match {
                        case Nil => (left, west)
                        case _ => east match {
                          case Nil => (left, west)
                          case fish :: next =>
                            val (wastWest, nextWest) = west.partition(_ same fish)
                            reduce(next, nextWest, wastWest match {
                              case Nil => fish :: left
                              case _   => left
                            })
                        }
                      }
                      reduce(FishSizes.findByPhoto(thePhoto), report.fishes.toList) match {
                        case (dbFish, rpFish) =>
                          val dones = List(
                            dbFish.par.map(_.delete).filter(_ == true),
                            rpFish.par.map(_ add thePhoto).flatten
                          ).par.map(_.size)
                          Logger info f"Update ${FishSizes.tableName}: ${dones(0)} deleted, ${dones(1)} added"
                          Ok
                      }
                    case Nil  => InternalServerError(f"Not Found photo for ${doneCR}")
                    case many => InternalServerError(f"Too Many photo for ${doneCR}: ${many}")
                  }
              }
          }
      }
    }
  }

  def remove(ticket: String) = Action.async(parse.json((
    (__ \ "id").read[Long]
  ))) { implicit request =>
    val id = request.body
    Logger debug f"Deleting report: id=${id}"
    Future {
      Ok
    }
  }

  def add(ticket: String) = Action.async(parse.json((
    (__ \ "report").read[Report]
  ))) { implicit request =>
    val report = request.body
    Logger debug f"Adding ${report}"
    Future {
      Ok
    }
  }
}
