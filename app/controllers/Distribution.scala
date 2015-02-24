package controllers

import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{ Action, Controller }

import models.Distributions

object Distribution extends Controller {
  def mine(ticket: String) = Action.async {
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          val catches = Distributions.catches(Some(ticket.userId))
          Ok(catches.asJson)
      }
    }
  }
  def others(ticket: String) = Action.async {
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          val catches = Distributions.catches(None)
          Ok(catches.asJson)
      }
    }
  }
  def names(ticket: String) = Action.async {
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          val names = Distributions.names
          Ok(names.asJson)
      }
    }
  }
}