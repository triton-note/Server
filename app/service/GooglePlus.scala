package service

import scala.{ Left, Right }
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.Logger
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.libs.ws.WS

import org.fathens.play.util.Exception.allCatch

import com.google.api.client.googleapis.auth.oauth2.GoogleCredential
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.services.plus.Plus
import com.google.api.services.plus.model.Person

import models.db.{ Image, User }

object GooglePlus {
  val applicationName = System.getenv("GOOGLEPLUS_APPLICATION_NAME")
  val apiKey = System.getenv("GOOGLEPLUS_API_KEY")

  object ActivityTypes {
    val AddActivity = "http://schemas.google.com/AddActivity"
  }

  def getService(token: String) = {
    val credential = new GoogleCredential().setAccessToken(token)
    new Plus.Builder(new NetHttpTransport, new JacksonFactory, credential)
      .setApplicationName(applicationName)
      .build
  }

  def getUserByToken(token: String): Option[User] = {
    for {
      me <- findMe(token)
      email <- getEmail(me)
    } yield User.findBy(email) getOrElse {
      val firstName = me.getName.getGivenName
      val lastName = List(Option(me.getName.getMiddleName), Option(me.getName.getFamilyName)).flatten.mkString(" ")
      val avatarUrl = Option(me.getImage.getUrl)
      User.addNew(email, None, firstName, lastName, avatarUrl)
    }
  }

  def findMe(token: String): Option[Person] = allCatch opt {
    getService(token).people().get("me").execute
  }

  def getEmail(me: Person): Option[String] = {
    val rc = me.getEmails.filter(_.getType == "account").map(_.getValue)
    Logger debug f"Emails as account: ${rc}"
    rc.headOption
  }

  def publish(token: String)(photo: Image, title: String, message: Option[String]) = {
    val itemId = play.api.libs.Codecs.sha1(photo.id)
    val desc = (message getOrElse "")
    val json = Json.obj(
      "kind" -> "plus#moment",
      "type" -> ActivityTypes.AddActivity,
      "object" -> Json.obj(
        "kind" -> "plus#itemScope",
        "type" -> ActivityTypes.AddActivity,
        "id" -> itemId,
        "name" -> title,
        "description" -> desc
      )
    )
    val client = WS.client url "https://www.googleapis.com/plus/v1/people/me/moments/vault"
    client.withQueryString("key" -> apiKey)
      .withMethod("POST").withHeaders(
        "Content-Type" -> "application/json",
        "Authorization" -> f"Bearer ${token}"
      ).withBody(json).execute.map { res =>
          res.status match {
            case 200 => Right(res.json)
            case _   => Left(res.json)
          }
        }
  }
}
