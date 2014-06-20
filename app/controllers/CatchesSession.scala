package controllers

import scala.{Left, Right}
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scalaz.Scalaz._

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.functional.syntax.{functionalCanBuildApplicative, toFunctionalBuilderOps, unlift}
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.json.__
import play.api.mvc.{Action, Controller}

import models.{GeoInfo, Record, Settings}
import models.db.{CatchReports, FishSizes, Image, Images, Photos, User, VolatileTokens}
import service.InferenceCatches

object CatchesSession extends Controller {
  implicit class FoldableForm[T](form: Form[T]) {
    def lr = form.fold(
      (f) => Left(Future(BadRequest(f.errors.mkString("\n")))),
      Right(_))
  }
  object SessionValue {
    implicit val sessionOptionFormat =
      (
        (__ \ "user").format[Option[User]] and
        (__ \ "geoinfo").formatNullable[GeoInfo] and
        (__ \ "photo").formatNullable[Option[Image]] and
        (__ \ "record").formatNullable[Record]
      )(
          (user, geoinfo, photo, record) => for {
            u <- user
            p <- photo
          } yield SessionValue(u, geoinfo, p, record),
          unlift((vOpt: Option[SessionValue]) => vOpt.map {
            v => (Some(v.user), v.geoinfo, Some(v.photo), v.record)
          }))
  }
  case class SessionValue(
    user: User,
    geoinfo: Option[GeoInfo],
    photo: Option[Image] = None,
    record: Option[Record] = None) {
    override def toString = Json.toJson(Option(this)).toString
  }
  def start = Action.async { implicit request =>
    val form = Form(tuple(
      "ticket" -> nonEmptyText,
      "geoinfo" -> optional(
        mapping(
          "latitude" -> bigDecimal,
          "longitude" -> bigDecimal
        )(GeoInfo.apply)(GeoInfo.unapply)
      )
    )).bindFromRequest.lr
    form match {
      case Left(res) => res
      case Right((ticket, geoinfo)) => Future {
        val ok = for {
          vt <- VolatileTokens get ticket
          extra <- vt.extra
          user <- (Json.parse(extra) \ "user").as[Option[User]]
        } yield {
          val value = SessionValue(user, geoinfo)
          val session = VolatileTokens.createNew(Settings.Session.timeoutUpload, Option(value.toString))
          Ok(session.id)
        }
        ok getOrElse BadRequest("Ticket Expired")
      }
    }
  }
  def addPhoto = Action.async(parse.multipartFormData) { implicit request =>
    val form = {
      val sessions = request.body.dataParts("session")
      val photoFiles = request.body.files.map(_.ref.file)
      Seq(
        sessions.isEmpty option "session is empty",
        photoFiles.isEmpty option "Photo is not uploaded"
      ).flatten match {
          case Nil    => Right(sessions.head, photoFiles.head)
          case errors => Left(Future(BadRequest(errors.mkString("\n"))))
        }
    }
    form match {
      case Left(res) => res
      case Right((session, file)) => Future {
        val ok = for {
          vt <- VolatileTokens get session
          extra <- vt.extra
          value <- Json.parse(extra).as[Option[SessionValue]]
          image <- Images.addNew(file)
        } yield {
          vt.setExtra(value.copy(photo = Some(image)).toString)
          val catches = InferenceCatches.infer(image.file, value.geoinfo)
          val res = Json.arr(catches)
          Ok(res.toString)
        }
        ok getOrElse BadRequest("Session Expired")
      }
    }
  }
  def submit = Action.async { implicit request =>
    val form = Form(tuple(
      "session" -> nonEmptyText,
      "record" -> nonEmptyText
    )).bindFromRequest.lr
    form match {
      case Left(res) => res
      case Right((session, json)) => Future {
        val ok = for {
          vt <- VolatileTokens get session
          extra <- vt.extra
          value <- Json.parse(extra).as[Option[SessionValue]]
        } yield {
          val given = Json.parse(json).as[Record]
          val ok = for {
            photo <- value.photo
            report <- commit(photo, given)(value.user)
          } yield Ok
          ok getOrElse InternalServerError("Failed to commit the submit")
        }
        ok getOrElse BadRequest("Session Expired")
      }
    }
  }
  def commit(photo: Image, given: Record)(implicit user: User) = {
    for {
      report <- CatchReports.addNew(user, given.geoinfo.get, given.date)
      photo <- Photos.addNew(report, photo)
    } yield {
      given.catches.map { fish =>
        FishSizes.addNew(photo, fish.name, fish.count, fish.weight, fish.length)
      }
      report.addComment(given.comment)
      report
    }
  }
  def publish = Action.async { implicit request =>
    val form = Form(tuple(
      "session" -> nonEmptyText,
      "way" -> nonEmptyText,
      "token" -> nonEmptyText
    )).bindFromRequest.lr
    form match {
      case Left(res) => res
      case Right((session, way, token)) => {
        way match {
          case "facebook" => {
            val ok = for {
              vt <- VolatileTokens get session
              extra <- vt.extra
              value <- Json.parse(extra).as[Option[SessionValue]]
            } yield {

              Future(NotImplemented)
            }
            ok getOrElse Future(BadRequest("Session Expired"))
          }
          case _ => Future(NotImplemented)
        }
      }
    }
  }
}