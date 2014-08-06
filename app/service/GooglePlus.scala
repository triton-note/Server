package service

import scala.{ Left, Right }
import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

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
import com.google.api.services.plus.model.{ ItemScope, Moment, Person }

import models.db.{ Image, ImageRelation, Photo, User }

object GooglePlus {
  val applicationName = System.getenv("GOOGLEPLUS_APPLICATION_NAME")
  val apiKey = System.getenv("GOOGLEPLUS_API_KEY")

  object ActivityTypes {
    val AddActivity = "https://schemas.google.com/AddActivity"
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
      val lastName = List(me.getName.getMiddleName, me.getName.getFamilyName).flatMap(Option(_)).mkString(" ")
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

  def publish(token: String)(photo: Photo, title: String, message: Option[String]) = allCatch opt {
    val itemId = play.api.libs.Codecs.sha1(photo.id)
    val desc = (message getOrElse "")
    implicit class ImageToURL(oi: Image) {
      def toURL = oi.url(1 hour).toString
    }
    val image = photo.image.get
    val moment = new Moment().setType(ActivityTypes.AddActivity).setTarget(new ItemScope()
      .setId(itemId)
      .setType(ActivityTypes.AddActivity)
      .setName(title)
      .setDescription(desc)
      .setImage(image.toURL)
    )
    getService(token).moments().insert("me", "vault", moment).execute()
  }
}
