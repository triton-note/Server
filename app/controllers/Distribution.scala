package controllers

import scala.concurrent.Future

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.{ Action, Controller }

import models.{ Distributions, User }

object Distribution extends Controller {
  def mine(ticket: String) = Action.async {
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) => User get ticket.userId match {
          case None => BadRequest(f"User not found: ${ticket.userId}")
          case Some(user) =>
            val catches = Distributions.catches(Some(user))
            Ok(Json toJson catches)
        }
      }
    }
  }
  def others(ticket: String) = Action.async {
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          val catches = Distributions.catches(None)
          Ok(Json toJson catches)
      }
    }
  }
  def names(ticket: String) = Action.async {
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) =>
          val names = Distributions.names()
          Ok(Json toJson names)
      }
    }
  }
}