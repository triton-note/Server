package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.libs.json._

import play.api.mvc.{ Action, Controller }
import models.Distributions

object Distribution extends Controller {
  def mine(ticket: String) = Action.async {
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => TicketExpired
        case Some((vt, value, user)) =>
          val catches = Distributions.catches(Some(user))
          Ok(Json toJson catches)
      }
    }
  }
  def others(ticket: String) = Action.async {
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => TicketExpired
        case Some((vt, value, user)) =>
          val catches = Distributions.catches(None)
          Ok(Json toJson catches)
      }
    }
  }
  def names(ticket: String) = Action.async {
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => TicketExpired
        case Some((vt, value, user)) =>
          val names = Distributions.names()
          Ok(Json toJson names)
      }
    }
  }
}