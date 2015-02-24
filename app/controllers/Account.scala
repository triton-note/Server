package controllers

import scala.concurrent.Future

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{ Action, Controller }

import org.fathens.play.util.Exception.allCatch

import models.{ User, ValueUnit, VolatileToken }
import service.{ Facebook, Settings }

object Account extends Controller {
  def login(way: String) = Action.async(parse.json(
    (__ \ "accessKey").read[String]
  )) { implicit request =>
    val accessKey = request.body
    Logger info f"Authorizing ${way}"
    allCatch opt User.SocialConnection.Service.withName(way) match {
      case None => Future(BadRequest(f"Invalid social service: ${way}"))
      case Some(service) => (service match {
        case User.SocialConnection.Service.FACEBOOK => Facebook.User(Facebook.AccessKey(accessKey))
        case _                                      => Future(None)
      }) map { u =>
        Logger debug f"Authorized user from $way: $u"
        u map { user =>
          val ticket = TicketValue(user.id, way, accessKey)
          val vt = VolatileToken.create(ticket, Settings.Session.timeoutTicket)
          Ok(vt.id)
        } getOrElse Unauthorized
      }
    }
  }

  def connect(way: String) = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "accessKey").read[String]
  ).tupled)) { implicit request =>
    val (ticket, accessKey) = request.body
    allCatch opt User.SocialConnection.Service.withName(way) match {
      case None => Future(BadRequest(f"Invalid social service: ${way}"))
      case Some(service) => ticket.asToken[TicketValue] match {
        case None => Future(TicketExpired)
        case Some((vt, ticket)) => User get ticket.userId match {
          case None => Future(BadRequest(f"User not found: ${ticket.userId}"))
          case Some(user) => (service match {
            case User.SocialConnection.Service.FACEBOOK => Facebook.User.connect(user)(Facebook.AccessKey(accessKey))
            case _                                      => Future(None)
          }) map { so =>
            so match {
              case Some(social) => Ok
              case None         => BadRequest
            }
          }
        }
      }
    }
  }

  def disconnect(way: String) = Action.async(parse.json(
    (__ \ "ticket").read[String]
  )) { implicit request =>
    Future {
      val ticket = request.body
      allCatch opt User.SocialConnection.Service.withName(way) match {
        case None => BadRequest(f"Invalid social service: ${way}")
        case Some(service) => ticket.asToken[TicketValue] match {
          case None => TicketExpired
          case Some((vt, ticket)) => User get ticket.userId match {
            case None => BadRequest(f"User not found: ${ticket.userId}")
            case Some(user) => user.connections.find(_.service == service).filter(_.connected) match {
              case None => BadRequest(f"Not connected: ${service}")
              case Some(social) =>
                val add = social.copy(connected = true)
                val left = user.connections.filter(_.service != service)
                user.copy(connections = left + add).save match {
                  case None    => InternalServerError(f"Failed to disconnect ${social}")
                  case Some(_) => Ok
                }
            }
          }
        }
      }
    }
  }

  def loadMeasures = Action.async(parse.json(
    (__ \ "ticket").read[String]
  )) { implicit request =>
    val ticket = request.body
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) => User get ticket.userId match {
          case None       => BadRequest(f"User not found: ${ticket.userId}")
          case Some(user) => Ok(user.measureUnit.asJson)
        }
      }
    }
  }

  def updateMeasures = Action.async(parse.json((
    (__ \ "ticket").read[String] and
    (__ \ "measures").read[ValueUnit.Measures]
  ).tupled)) { implicit request =>
    val (ticket, measureUnit) = request.body
    Future {
      ticket.asToken[TicketValue] match {
        case None => TicketExpired
        case Some((vt, ticket)) => User get ticket.userId match {
          case None => BadRequest(f"User not found: ${ticket.userId}")
          case Some(user) =>
            user.copy(measureUnit = measureUnit).save match {
              case None     => InternalServerError(f"Failed to update user: ${user.id}")
              case Some(ok) => Ok
            }
        }
      }
    }
  }
}
