package models

import play.Logger
import dispatch._
import Defaults._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import com.ning.http.multipart._

object Facebook {
  val fb = host("graph.facebook.com").secure
  case class AccessKey(token: String)
  case class ObjectId(id: String)
  def part(file: Storage.S3File): FilePart = {
    val source = new PartSource {
      def createInputStream = file.read
      def getFileName = file.name
      def getLength = file.length
    }
    new FilePart(file.name, source)
  }
  object User {
    /**
     * Obtain attributes of user by accessKey.
     * The attributes is specified by fields.
     */
    def obtain(fields: String*)(implicit accesskey: AccessKey): Future[JsValue] = {
      val req = (fb / "me").GET << Map(
        "fields" -> fields.mkString(","),
        "access_token" -> accesskey.token)
      Http(req OK as.String).map(Json.parse)
    }
    /**
     * Find UserAlias by given accessKey.
     * If accessKey is not valid, return None.
     * If UserAlias is not found, return email which is obtained by accessKey.
     * If UserAlias is found by email, return UserAlias.
     */
    def find(implicit accesskey: AccessKey): Future[Option[Either[String, db.UserAlias]]] = {
      obtain("email") map { json =>
        for {
          email <- (json \ "email").asOpt[String]
        } yield db.UserAlias.get(email, db.UserAliasDomain.facebook) match {
          case Some(user) => Right(user)
          case None       => Left(email)
        }
      }
    }
    /**
     * Create UserAlias by email.
     * The email is obtained by accessKey.
     */
    def create(implicit accesskey: AccessKey): Future[Option[db.UserAlias]] = {
      obtain("email", "first_name", "last_name", "picture") map { json =>
        for {
          email <- (json \ "email").asOpt[String]
          firstName <- (json \ "first_name").asOpt[String]
          lastName <- (json \ "last_name").asOpt[String]
        } yield {
          val avatarUrl = (json \ "picture" \ "data" \ "url").asOpt[String]
          val user = db.User.addNew(firstName, lastName, avatarUrl)
          Logger.info(f"Creating alias '$email' of $user as facebook and email at once")
          def as(f: db.UserAliasDomain.type => String) = db.UserAlias.addNew(user, email, f(db.UserAliasDomain), 0)
          as(_.email)
          as(_.facebook)
        }
      }
    }
    /**
     * Find UserAlias by email which is obtained by accessKey.
     * If UserAlias is not created yet, create it.
     * If accessKey is not valid, return None.
     */
    def apply(accesskey: String): Future[Option[db.UserAlias]] = {
      implicit val ak = AccessKey(accesskey)
      find flatMap {
        _ match {
          case Some(e) => e match {
            case Right(user) => Future(Some(user))
            case Left(email) => create
          }
          case None => Future(None)
        }
      }
    }
  }
  object Publish {
    implicit val jsonNameReader = (
      (__ \ "id").read[String] ~
      (__ \ "name").read[String]
    ) tupled
    def findAlbum(name: String)(implicit accessKey: AccessKey): Future[Option[ObjectId]] = {
      def find = {
        val req = (fb / "me" / "albums").GET << Map(
          "access_token" -> accessKey.token)
        Http(req OK as.String).map(Json.parse).map(_ \ "data").map(_.as[List[(String, String)]])
      }
      for {
        albums <- find
      } yield {
        val list = for {
          (albumId, albumName) <- albums
          if (albumName == name)
        } yield ObjectId(albumId)
        list.headOption
      }
    }
    def getAlbumOrCreate(name: String, message: Option[String] = None)(implicit accessKey: AccessKey): Future[ObjectId] = {
      for {
        found <- findAlbum(name)
        id <- if (found.isEmpty) makeAlbum(name, message) else Future(found.get)
      } yield id
    }
    def makeAlbum(name: String, message: Option[String] = None)(implicit accessKey: AccessKey): Future[ObjectId] = {
      val mes = (message.map(m => Map("message" -> m)) getOrElse Map())
      val req = (fb / "me" / "album").POST << mes << Map(
        "access_token" -> accessKey.token,
        "name" -> name
      )
      for {
        json <- Http(req OK as.String).map(Json.parse)
      } yield {
        ObjectId((json \ "id").as[String])
      }
    }
    def addPhoto(albumId: ObjectId)(file: Storage.S3File, message: Option[String] = None)(implicit accessKey: AccessKey): Future[ObjectId] = {
      val req = {
        val r = (fb / albumId.id / "photo").POST addBodyPart part(file)
        message match {
          case None    => r
          case Some(m) => r addBodyPart new StringPart("message", m)
        }
      }
      for {
        json <- Http(req OK as.String).map(Json.parse)
      } yield {
        ObjectId((json \ "id").as[String])
      }
    }
  }
}
