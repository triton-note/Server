package controllers

import java.util.Date

import scala.concurrent.Future

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller }

import models.GeoInfo
import service.NaturalConditions

object Conditions extends Controller {

  def get = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "date").read[Date] and
    (__ \ "geoinfo").read[GeoInfo]
  ).tupled)) { implicit request =>
    val (ticket, date, geoinfo) = request.body
    Logger debug f"Getting conditions: ${geoinfo} at ${date}"
    ticket.asToken[TicketValue] match {
      case None => Future(TicketExpired)
      case Some((vt, ticket)) =>
        NaturalConditions.at(date, geoinfo).map { condition =>
          Ok(condition.asJson)
        }
    }
  }
}
