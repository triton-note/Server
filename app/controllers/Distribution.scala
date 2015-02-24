package controllers

import scala.concurrent.Future

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{ Action, Controller }

import models.Distributions

object Distribution extends Controller {
  def mine = Action.async(parse.json(
    (__ \ "ticket").read[String]
  )) { implicit request =>
    val ticket = request.body
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          val catches = Distributions.catches(Some(ticket.userId))
          Ok(catches.asJson)
      }
    }
  }
  def others = Action.async(parse.json(
    (__ \ "ticket").read[String]
  )) { implicit request =>
    val ticket = request.body
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          val catches = Distributions.catches(None)
          Ok(catches.asJson)
      }
    }
  }
  def monakers = Action.async(parse.json(
    (__ \ "ticket").read[String]
  )) { implicit request =>
    val ticket = request.body
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          val monakers = Distributions.monakers
          Ok(monakers.asJson)
      }
    }
  }
}