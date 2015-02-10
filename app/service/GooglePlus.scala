package service

import scala.concurrent.duration.DurationInt

import play.api.Logger

import org.fathens.play.util.Exception.allCatch

import com.google.api.client.googleapis.auth.oauth2.GoogleCredential
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.services.plus.Plus
import com.google.api.services.plus.model.{ ItemScope, Moment, Person }

import models.db.{ Image, Photo, SocialConnection, User }

object GooglePlus {
  val applicationName = Settings.GOOGLEPLUS_APPLICATION_NAME
  val apiKey = Settings.GOOGLEPLUS_API_KEY

  object ActivityTypes {
    val AddActivity = "https://schemas.google.com/AddActivity"
  }

  def getUserByToken(token: String): Option[User] = {
    for {
      me <- findMe(token)
      user <- findSocial(me).map(_.user.get) orElse create(me)
    } yield user
  }
  def connect(user: User, token: String): Option[SocialConnection] = {
    findMe(token).map { me =>
      findSocial(me) getOrElse {
        val social = SocialConnection.addNew(me.getId, SocialConnection.Service.GOOGLE, user)
        Logger.info(f"Connecting ${user} to ${social}")
        social
      }
    }
  }

  def getService(token: String) = {
    val credential = new GoogleCredential().setAccessToken(token)
    new Plus.Builder(new NetHttpTransport, new JacksonFactory, credential)
      .setApplicationName(applicationName)
      .build
  }
  def findSocial(me: Person): Option[SocialConnection] = {
    SocialConnection.findBy(me.getId, SocialConnection.Service.GOOGLE)
  }
  def create(me: Person): Option[User] = {
    val name = me.getDisplayName
    val user = User.addNew(name)
    val social = SocialConnection.addNew(me.getId, SocialConnection.Service.GOOGLE, user)
    Logger.info(f"Creating ${user} as ${social}")
    Option(user)
  }

  def findMe(token: String): Option[Person] = allCatch opt {
    getService(token).people().get("me").execute
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
