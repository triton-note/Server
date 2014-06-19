package controllers

import scala.{ Left, Right }
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import scalaz.Scalaz._

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.{ Action, Controller }

import models.{ GeoInfo, Settings, Storage }
import models.db.{ User, Users, VolatileToken, VolatileTokens }
import service.InferencePreInfo

object RecordSession extends Controller {
  implicit class FoldableForm[T](form: Form[T]) {
    def lr = form.fold(
      (f) => Left(Future(BadRequest(f.errors.mkString("\n")))),
      Right(_))
  }
  def getUser(vt: VolatileToken) = {
    for {
      json <- vt.extra
      id <- (Json.parse(json) \ "user").asOpt[String]
      user <- Users get id
    } yield user
  }
  case class FishInfo(
    name: String,
    count: Int,
    weight: Option[Double],
    length: Option[Double]) {
    def toJson = Json.obj(
      "name" -> name,
      "count" -> count,
      "weight" -> weight,
      "length" -> length
    )
  }
  def toJson(user: User, geoinfo: Option[GeoInfo], photo: Option[Storage.S3File] = None, catches: List[FishInfo] = Nil) = Json.obj(
    "user" -> user.id,
    "geoinfo" -> geoinfo.map(_.toJson),
    "photo" -> photo.map(_.path),
    "catches" -> catches.map(_.toJson)
  )
  def fromJson(json: String) = {
    val top = Json.parse(json)
    val user = Users get (top \ "user").as[String]
    val geoinfo = (top \\ "geoinfo").map(GeoInfo.fromJson).flatten.headOption
    val photo = (top \ "photo").asOpt[String].map { path =>
      path
    }
    val catches = (top \\ "catches").map { jv =>
      for {
        name <- (jv \ "name").asOpt[String]
        count <- (jv \ "count").asOpt[Int]
      } yield {
        val weight = (jv \ "weight").asOpt[Double]
        val length = (jv \ "length").asOpt[Double]
        FishInfo(name, count, weight, length)
      }
    }.flatten.toList
    (user, geoinfo, photo, catches)
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
        VolatileTokens.get(ticket).flatMap(getUser).map { user =>
          val value = toJson(user, geoinfo)
          val ticket = VolatileTokens.createNew(Settings.Session.timeoutUpload, Option(value.toString))
          Ok(ticket.id)
        } getOrElse BadRequest("Ticket Expired")
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
      case Right((session, photoFile)) => Future {
        val ok = for {
          vt <- VolatileTokens get session
          user <- getUser(vt)
          extra <- vt.extra
          (userO, geoinfo, _, catches) = fromJson(extra)
          user <- userO
        } yield {
          val file = Storage.file(user.id, session)
          file.write(photoFile)
          vt.setExtra(toJson(user, geoinfo, Option(file), catches))
          val infos = InferencePreInfo.infer(file, geoinfo)
          val res = Json.arr(infos.map(_.toJson))
          Ok(res.toString)
        }
        ok getOrElse BadRequest("Session Expired")
      }
    }
  }
  def submit = Action.async { implicit request =>
    val form = Form(tuple(
      "session" -> nonEmptyText
    )).bindFromRequest.lr
    
    Future(NotImplemented)
  }
  def publish = Action.async { implicit request =>
    Future(NotImplemented)
  }
}