package controllers

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller }

import models.Report
import models.db.{ CatchReport, Comment, FishSize, Photo }

object ReportSync extends Controller {

  def load(ticket: String) = Action.async(parse.json((
    (__ \ "count").read[Int] and
    (__ \ "last").readNullable[String]
  ).tupled)) { implicit request =>
    val (count, last) = request.body
    Logger debug f"Loading reports: count(${count}) from ${last}"
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          val reports = CatchReport.find(
            _.withIndexName("USER-TIMESTAMP-index").withKeyConditions(Map(
              CatchReport.user compare Some(user)
            )).withScanIndexForward(false).withLimit(count).withExclusiveStartKey(
              last.flatMap(CatchReport.get) match {
                case Some(c) => c.toMap(CatchReport.id, CatchReport.user, CatchReport.timestamp)
                case None    => null
              })
          ).map { report =>
              val comment = report.comments.find(_.user == user).map(_.text) getOrElse ""
              val photos = Photo.find(
                _.withIndexName("CATCH_REPORT-index").withKeyConditions(Map(
                  Photo.catchReport compare Some(report)
                )))
              val fishes = photos.flatMap { photo =>
                FishSize.find(
                  _.withIndexName("PHOTO-CREATED_AT-index").withKeyConditions(Map(
                    FishSize.photo compare Some(photo)
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
          report.id.flatMap(CatchReport.get) match {
            case None => BadRequest(f"Invalid id: ${report.id.orNull}")
            case Some(src) =>
              CatchReport.update(src.id, List(
                CatchReport.timestamp.diff(src.timestamp, report.dateAt),
                CatchReport.location.diff(src.location, report.location.name),
                CatchReport.latitude.diff(src.latitude, report.location.geoinfo.latitude.toDouble),
                CatchReport.latitude.diff(src.longitude, report.location.geoinfo.longitude.toDouble)
              ).flatten.toMap) match {
                case None => InternalServerError("Failed to update report")
                case Some(doneCR) =>
                  doneCR.comments.find(_.user == user).headOption.flatMap { comment =>
                    Comment.update(comment.id, List(
                      Comment.text.diff(comment.text, report.comment)
                    ).flatten.toMap)
                  }
                  Photo.findByCatchReport(doneCR) match {
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
                      reduce(FishSize.findByPhoto(thePhoto), report.fishes.toList) match {
                        case (dbFish, rpFish) =>
                          val dones = List(
                            dbFish.par.map(_.delete).filter(_ == true),
                            rpFish.par.map(_ add thePhoto)
                          ).par.map(_.size)
                          Logger info f"Update ${FishSize.tableName}: ${dones(0)} deleted, ${dones(1)} added"
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
    (__ \ "id").read[String]
  ))) { implicit request =>
    val id = request.body
    Logger debug f"Deleting report: id=${id}"
    Future {
      CatchReport.get(id) match {
        case None => BadRequest(f"Invalid id: ${id}")
        case Some(cr) => Photo.findByCatchReport(cr) match {
          case thePhoto :: Nil => thePhoto.image match {
            case None => InternalServerError(f"Not Found image for ${thePhoto}")
            case Some(theImage) =>
              val fishes = FishSize.findByPhoto(thePhoto)
              val targets = cr :: thePhoto :: theImage :: (fishes: List[{ def delete: Boolean }])
              Logger debug f"Deleting: ${targets}"
              if (targets.par.map(_.delete).forall(_ == false)) InternalServerError("Failed to delete any items") else Ok
          }
          case Nil  => InternalServerError(f"Not Found photo for ${cr}")
          case many => InternalServerError(f"Too Many photo for ${cr}: ${many}")
        }
      }
    }
  }
}
