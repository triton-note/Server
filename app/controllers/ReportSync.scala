package controllers

import scala.concurrent.Future

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{ Action, Controller }

import models.Report
import models.db.{ CatchReport, Comment, FishSize, Photo }

object ReportSync extends Controller {

  def convert(cr: CatchReport): Option[Report] = {
    Photo.findBy(cr).headOption.map { photo =>
      val comment = cr.topComment.map(_.text) getOrElse ""
      val fishes = FishSize.findBy(photo).toSeq.map { fish =>
        Report.Fishes(fish.name, fish.count.toInt, fish.weight.map(Report.ValueUnit.tupled), fish.length.map(Report.ValueUnit.tupled))
      }
      Report(Some(cr.id),
        comment,
        cr.timestamp,
        Report.Location(cr.location, cr.geoinfo),
        cr.condition,
        photo.group.map(_.asURL),
        fishes)
    }
  }

  def load(ticket: String) = Action.async(parse.json((
    (__ \ "count").read[Int] and
    (__ \ "last").readNullable[String]
  ).tupled)) { implicit request =>
    val (count, last) = request.body
    Logger debug f"Loading reports: count(${count}) from ${last}"
    ticket.asTokenOfUser[TicketValue] match {
      case None => Future(TicketExpired)
      case Some((vt, value, user)) => Future {
        val reports = CatchReport.findBy(user, count, last).flatMap(convert)
        Ok(Json toJson reports)
      }
    }
  }

  def read(ticket: String) = Action.async(parse.json((
    (__ \ "id").read[String]
  ))) { implicit request =>
    val id = request.body
    Logger debug f"Reading report:${id}"
    ticket.asTokenOfUser[TicketValue] match {
      case None => Future(TicketExpired)
      case Some((vt, value, user)) => Future {
        CatchReport.get(id).flatMap(convert) match {
          case None => BadRequest(f"Report NotFound: ${id}")
          case Some(report) => Ok {
            Json.obj(
              "report" -> report)
          }
        }
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
        case None => TicketExpired
        case Some((vt, value, user)) =>
          report.id.flatMap(CatchReport.get) match {
            case None => BadRequest(f"Invalid id: ${report.id.orNull}")
            case Some(src) =>
              CatchReport.update(src.id, List(
                src.diff(_.timestamp, report.dateAt),
                src.diff(_.location, report.location.name),
                src.diff(_.condition, report.condition),
                src.diff(_.latitude, report.location.geoinfo.latitude.toDouble),
                src.diff(_.longitude, report.location.geoinfo.longitude.toDouble)
              ).flatten.toMap) match {
                case None => InternalServerError("Failed to update report")
                case Some(doneCR) =>
                  doneCR.topComment.flatMap { comment =>
                    Comment.update(comment.id, List(
                      comment.diff(_.text, report.comment)
                    ).flatten.toMap)
                  }
                  Photo.findBy(doneCR) match {
                    case thePhoto :: Nil =>
                      def reduce(left: List[FishSize], adding: List[Report.Fishes], deleting: List[FishSize] = Nil): (List[FishSize], List[Report.Fishes]) = left match {
                        case Nil => (deleting, adding)
                        case fish :: next => adding.partition(_ same fish) match {
                          case (Nil, nextAdding) => reduce(next, nextAdding, fish :: deleting)
                          case (_, nextAdding)   => reduce(next, nextAdding, deleting)
                        }
                      }
                      reduce(FishSize.findBy(thePhoto), report.fishes.toList) match {
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
        case Some(cr) => Photo.findBy(cr) match {
          case thePhoto :: Nil => thePhoto.image match {
            case None => InternalServerError(f"Not Found image for ${thePhoto}")
            case Some(theImage) =>
              val fishes = FishSize.findBy(thePhoto)
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
