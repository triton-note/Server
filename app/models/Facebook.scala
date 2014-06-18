package models

import scala.{Left, Right}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.control.Exception.allCatch

import play.api.Logger
import play.api.Play.current
import play.api.libs.functional.syntax.{functionalCanBuildApplicative, toFunctionalBuilderOps}
import play.api.libs.json.{JsValue, __}
import play.api.libs.ws.{WS, WSResponse}

import com.ning.http.client.{FilePart, RequestBuilder, StringPart}

object Facebook {
  object fb {
    val host = "https://graph.facebook.com"
    val client = WS.client
    def /(path: String) = client url "${host}/${path}"
  }
  case class AccessKey(token: String)
  case class ObjectId(id: String)
  def part(file: Storage.S3File) = {

  }
  object parse {
    def ObjectID(res: WSResponse) = ObjectId((res.json \ "id").as[String])
    def NameID(res: WSResponse): List[(String, String)] = {
      implicit val jsonNameReader = {
        (
          (__ \ "id").read[String] ~
          (__ \ "name").read[String]) tupled
      }
      (res.json \ "data").as[List[(String, String)]]
    }
  }
  object User {
    /**
     * Obtain attributes of user by accessKey.
     * The attributes is specified by fields.
     */
    def obtain(fields: String*)(implicit accesskey: AccessKey): Future[Option[JsValue]] = {
      (fb / "me").withQueryString(
        "fields" -> fields.mkString(","),
        "access_token" -> accesskey.token).get().map {
          allCatch opt _.json
        }
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
            case None => Left(email)
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
      def find = (fb / "me/albums").withQueryString(
        "access_token" -> accessKey.token).get().map {
          allCatch opt parse.NameID(_)
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
          case None => makeAlbum(name, message)
          case Some(_) => Future(found)
        }
      } yield id
    }
    def makeAlbum(name: String, message: Option[String] = None)(implicit accessKey: AccessKey): Future[Option[ObjectId]] = {
      val body = message.map("message" -> Seq(_)).toMap
      (fb / "me/album").withQueryString(
        "access_token" -> accessKey.token,
        "name" -> name).post(body).map {
          allCatch opt parse.ObjectID(_)
        }
    }
    def addPhoto(albumId: ObjectId)(file: Storage.S3File, message: Option[String] = None)(implicit accessKey: AccessKey): Future[Option[ObjectId]] = {
      val body = Map(
        "message" -> message.toSeq,
        "photo" -> Seq(file.generateURL(24 * 365 hours).toString))
      (fb / "${albumId.id}/photo").post(body).map {
        allCatch opt parse.ObjectID(_)
      }
    }
  }
}
