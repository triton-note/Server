package models

import play.Logger
import dispatch._
import Defaults._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import com.ning.http.multipart._

object Facebook {
  def fb = host("graph.facebook.com").secure
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
  object parse {
    import com.ning.http.client.Response
    def JSON(res: Response): JsValue = {
      val text = as.String(res)
      Logger.debug(f"Parsing json: $text")
      Json parse text
    }
    def ObjectID(res: Response): ObjectId = {
      ObjectId((JSON(res) \ "id").as[String])
    }
    def NameID(res: Response): List[(String, String)] = {
      implicit val jsonNameReader = {
        (
          (__ \ "id").read[String] ~
          (__ \ "name").read[String]
        ) tupled
      }
      (JSON(res) \ "data").as[List[(String, String)]]
    }
  }
  object User {
    /**
     * Obtain attributes of user by accessKey.
     * The attributes is specified by fields.
     */
    def obtain(fields: String*)(implicit accesskey: AccessKey): Future[Option[JsValue]] = {
      val req = (fb / "me").GET <<? Map(
        "fields" -> fields.mkString(","),
        "access_token" -> accesskey.token)
      Http(req OK parse.JSON).option
    }
    /**
     * Find UserAlias by given accessKey.
     * If accessKey is not valid, return None.
     * If UserAlias is not found, return email which is obtained by accessKey.
     * If UserAlias is found by email, return UserAlias.
     */
    def find(implicit accesskey: AccessKey): Future[Option[Either[String, db.User]]] = {
      obtain("email") map { opt =>
        for {
          json <- opt
          email <- (json \ "email").asOpt[String]
        } yield {
          Logger debug f"Getting UserAlias by email: $email"
          db.Users.find(email) match {
            case Some(user) => Right(user)
            case None       => Left(email)
          }
        }
      }
    }
    /**
     * Create User by email.
     * The email is obtained by accessKey.
     */
    def create(implicit accesskey: AccessKey): Future[Option[db.User]] = {
      obtain("email", "first_name", "last_name", "picture") map { opt =>
        for {
          json <- opt
          email <- (json \ "email").asOpt[String]
          firstName <- (json \ "first_name").asOpt[String]
          lastName <- (json \ "last_name").asOpt[String]
          avatarUrl = (json \ "picture" \ "data" \ "url").asOpt[String]
          user <- db.Users.addNew(email, None, firstName, lastName, avatarUrl)
        } yield {
          Logger.info(f"Creating alias '$email' of $user as facebook and email at once")
          user
        }
      }
    }
    /**
     * Find UserAlias by email which is obtained by accessKey.
     * If UserAlias is not created yet, create it.
     * If accessKey is not valid, return None.
     */
    def apply(accesskey: String): Future[Option[db.User]] = {
      implicit val ak = AccessKey(accesskey)
      Logger.debug(f"Login as user with $ak")
      find flatMap (_ match {
        case Some(e) => e match {
          case Right(user) => Future(Some(user))
          case Left(email) => create
        }
        case None => Future(None)
      })
    }
  }
  object Publish {
    def findAlbum(name: String)(implicit accessKey: AccessKey): Future[Option[ObjectId]] = {
      def find = {
        val req = (fb / "me" / "albums").GET << Map(
          "access_token" -> accessKey.token)
        Http(req OK parse.NameID).option
      }
      for {
        opt <- find
      } yield {
        val list = for {
          albums <- opt.toList
          (albumId, albumName) <- albums
          if (albumName == name)
        } yield ObjectId(albumId)
        list.headOption
      }
    }
    def getAlbumOrCreate(name: String, message: Option[String] = None)(implicit accessKey: AccessKey): Future[Option[ObjectId]] = {
      for {
        found <- findAlbum(name)
        id <- found match {
          case None    => makeAlbum(name, message)
          case Some(_) => Future(found)
        }
      } yield id
    }
    def makeAlbum(name: String, message: Option[String] = None)(implicit accessKey: AccessKey): Future[Option[ObjectId]] = {
      val mes = message.map("message" -> _).toMap
      val req = (fb / "me" / "album").POST << mes << Map(
        "access_token" -> accessKey.token,
        "name" -> name
      )
      Http(req OK parse.ObjectID).option
    }
    def addPhoto(albumId: ObjectId)(file: Storage.S3File, message: Option[String] = None)(implicit accessKey: AccessKey): Future[Option[ObjectId]] = {
      val req = {
        val r = (fb / albumId.id / "photo").POST addBodyPart part(file)
        message match {
          case None    => r
          case Some(m) => r addBodyPart new StringPart("message", m)
        }
      }
      Http(req OK parse.ObjectID).option
    }
  }
}
