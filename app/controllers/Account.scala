package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{ Action, Controller }

import org.fathens.play.util.Exception.allCatch

import models.Settings
import models.db.{ SocialConnection, VolatileToken }
import service.{ Facebook, GooglePlus }

object Account extends Controller {
  object Way {
    val facebook = SocialConnection.Service.FACEBOOK
    val google = SocialConnection.Service.GOOGLE
  }

  def login(way: String) = Action.async(parse.json(
    (__ \ "token").read[String]
  )) { implicit request =>
    val token = request.body
    Logger info f"Authorizing ${way}"
    allCatch opt SocialConnection.Service.withName(way) match {
      case None => Future(BadRequest(f"Invalid social service: ${way}"))
      case Some(service) => (service match {
        case Way.facebook => Facebook.User(Facebook.AccessKey(token))
        case Way.google   => Future(GooglePlus getUserByToken token)
        case _            => Future(None)
      }) map { u =>
        Logger debug f"Authorized user from $way: $u"
        u map { user =>
          val value = TicketValue(user.id, way, token)
          val ticket = VolatileToken.addNew(Settings.Session.timeoutTicket, Option(value.toString))
          Ok(ticket.id)
        } getOrElse Unauthorized
      }
    }
  }

  def connect(ticket: String) = Action.async(parse.json((
    (__ \ "way").read[String] and
    (__ \ "token").read[String]
  ).tupled)) { implicit request =>
    val (way, token) = request.body
    allCatch opt SocialConnection.Service.withName(way) match {
      case None => Future(BadRequest(f"Invalid social service: ${way}"))
      case Some(service) => ticket.asTokenOfUser[TicketValue] match {
        case None => Future(BadRequest("Ticket Expired"))
        case Some((vt, value, user)) => (service match {
          case Way.facebook => Facebook.User.connect(user)(Facebook.AccessKey(token))
          case Way.google   => Future(GooglePlus.connect(user, token))
          case _            => Future(None)
        }) map { so =>
          so match {
            case Some(social) => Ok
            case None         => BadRequest
          }
        }
      }
    }
  }

  def disconnect(ticket: String) = Action.async(parse.json(
    (__ \ "way").read[String]
  )) { implicit request =>
    Future {
      val way = request.body
      allCatch opt SocialConnection.Service.withName(way) match {
        case None => BadRequest(f"Invalid social service: ${way}")
        case Some(service) => ticket.asTokenOfUser[TicketValue] match {
          case None => BadRequest("Ticket Expired")
          case Some((vt, value, user)) => SocialConnection.findBy(user).find(_.service == service) match {
            case None => BadRequest(f"Not connected: ${service}")
            case Some(social) =>
              Logger info f"Deleting social: ${social}"
              if (social.delete) Ok else InternalServerError(f"Failed to delete: ${social}")
          }
        }
      }
    }
  }

  def loadProfile(ticket: String) = Action.async { implicit request =>
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          Ok(Json.obj(
            "email" -> user.email,
            "name" -> user.name,
            "avatar" -> user.avatarUrl
          ))
      }
    }
  }

  def changeProfile(ticket: String) = Action.async(parse.json((
    (__ \ "profile" \ "email").read[String] and
    (__ \ "profile" \ "name").read[String] and
    (__ \ "profile" \ "avatar").readNullable[String]
  ).tupled)) { implicit request =>
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          val (email, name, avatar) = request.body
          user.update(email = email, name = name, avatarUrl = avatar) match {
            case None     => InternalServerError(f"Failed to update user: ${user.id}")
            case Some(ok) => Ok
          }
      }
    }
  }

  def loadUnit(ticket: String) = Action.async { implicit request =>
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          Ok(Json.obj(
            "length" -> user.lengthUnit,
            "weight" -> user.weightUnit
          ))
      }
    }
  }

  def changeUnit(ticket: String) = Action.async(parse.json((
    (__ \ "unit" \ "length").read[String] and
    (__ \ "unit" \ "weight").read[String]
  ).tupled)) { implicit request =>
    Future {
      ticket.asTokenOfUser[TicketValue] match {
        case None => BadRequest("Ticket Expired")
        case Some((vt, value, user)) =>
          val (length, weight) = request.body
          user.update(lengthUnit = length, weightUnit = weight) match {
            case None     => InternalServerError(f"Failed to update user: ${user.id}")
            case Some(ok) => Ok
          }
      }
    }
  }
}
