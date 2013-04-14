package models

import java.sql.Date
import scala.slick.session.Session
import scala.slick.driver.PostgresDriver.simple._

object User extends Table[(String, Long, String, String, Date)]("USER") {
  def account = column[String]("account", O.PrimaryKey)
  def password = column[Long]("password", O.NotNull)
  def name = column[String]("name", O.NotNull)
  def email = column[String]("email", O.NotNull)
  def creation = column[Date]("creation", O.NotNull)
  def * = account ~ password ~ name ~ email ~ creation
}

object Photo extends Table[(Long, String, Date, Option[Long], Option[Long], String)]("PHOTO") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def path = column[String]("filepath", O.NotNull)
  def timestamp = column[Date]("timestamp", O.NotNull)
  def latitude = column[Long]("latitude", O.Nullable)
  def longitude = column[Long]("longitude", O.Nullable)
  def desc = column[String]("description", O.NotNull, O.Default(""))
  def * = id ~ path ~ timestamp ~ latitude.? ~ longitude.? ~ desc
  /**
   * Add new photo.
   * geoinfo is option.
   */
  def addNew(p: String, d: String, t: Option[Date] = None, geoinfo: Option[(Long, Long)] = None) = {
    DB withSession { implicit session: Session =>
      val time = t.getOrElse(DB.now)
      geoinfo match {
        case Some((lati, longi)) => (path ~ timestamp ~ latitude ~ longitude ~ desc).insert((p, time, lati, longi, d))
        case None                => (path ~ timestamp ~ desc).insert((p, time, d))
      }
    }
  }
  def all[F, T, G](f: Photo.type => F)(implicit shape: slick.lifted.Shape[F, T, G]) = {
    DB withSession { implicit session: Session =>
      Photo.map(f(_)).list
    }
  }
}

object Album extends Table[(Long, Option[Date], Option[String], Date, Date)]("ALBUM") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def date = column[Date]("date", O.Nullable)
  def point = column[String]("point", O.Nullable)
  def creation = column[Date]("creation", O.NotNull)
  def lastModified = column[Date]("lastModified", O.Nullable)
  def * = id ~ date.? ~ point.? ~ creation ~ lastModified
}

object PhotoAlbum extends Table[(Long, Long)]("PHOTO_ALBUM") {
  def photo = column[Long]("photo", O.NotNull)
  def album = column[Long]("album", O.NotNull)
  def * = photo ~ album
  def fkPhoto = foreignKey("photo_fk", photo, Photo)(_.id)
  def fkAlbum = foreignKey("album_fk", album, Album)(_.id)
}

object AlbumOwner extends Table[(Long, String)]("ALBUM_OWNER") {
  def album = column[Long]("album", O.NotNull)
  def user = column[String]("user", O.NotNull)
  def * = album ~ user
  def fkAlbum = foreignKey("album_fk", album, Album)(_.id)
  def fkUser = foreignKey("user_fk", user, User)(_.account)
}

object PhotoOwner extends Table[(Long, String)]("PHOTO_OWNER") {
  def photo = column[Long]("photo", O.NotNull)
  def user = column[String]("user", O.NotNull)
  def * = photo ~ user
  def fkPhoto = foreignKey("photo_fk", photo, Photo)(_.id)
  def fkUser = foreignKey("user_fk", user, User)(_.account)
}
