package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession
import models.GeoInfo
import models.Storage
import scala.concurrent.duration._

case class Photo(id: Long,
                 createdAt: Timestamp,
                 lastModifiedAt: Option[Timestamp],
                 path: String,
                 timestamp: Option[Timestamp],
                 geoinfo: Option[GeoInfo]) {
  import scala.concurrent.duration._
  def url(implicit limit: FiniteDuration = 1 minute) = Storage.file(path).generateURL(limit)
  /**
   * Prepared query for me
   */
  lazy val me = for {
    a <- Photo
    if (a.id === id)
  } yield a
  /**
   * Delete me
   */
  def delete = {
    DB withSession {
      me.delete
    }
  }
  /**
   * Change property (like a copy) and update Database
   */
  def update(thePath: String = path, theTimestamp: Option[Timestamp] = timestamp, theGeoinfo: Option[GeoInfo] = geoinfo): Photo = {
    val n = copy(lastModifiedAt = Some(DB.now), path = thePath, timestamp = theTimestamp, geoinfo = theGeoinfo)
    DB.withSession {
      me.map { a =>
        (a.lastModifiedAt.? ~ a.path ~ a.timestamp.? ~ a.latitude.? ~ a.longitude.?)
      }.update(n.lastModifiedAt, n.path, n.timestamp, n.geoinfo.map(_.latitude), n.geoinfo.map(_.longitude))
    }
    n
  }
  def bindTo(user: User) = DB withSession {
    PhotoOwner.addNew(this, user)._1
  }
  def bindTo(album: Album) = DB withSession {
    PhotoAlbum.addNew(this, album)._1
  }
}

object Photo extends Table[Photo]("PHOTO") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def path = column[String]("FILE_PATH", O.NotNull)
  def timestamp = column[Timestamp]("TIMESTAMP", O.Nullable)
  def latitude = column[Double]("LATITUDE", O.Nullable)
  def longitude = column[Double]("LONGITUDE", O.Nullable)
  // All columns
  def * = id ~ createdAt ~ lastModifiedAt.? ~ path ~ timestamp.? ~ latitude.? ~ longitude.? <> (
    { t => Photo(t._1, t._2, t._3, t._4, t._5, GeoInfo(t._6, t._7)) },
    { o: Photo => Some(o.id, o.createdAt, o.lastModifiedAt, o.path, o.timestamp, o.geoinfo.map(_.latitude), o.geoinfo.map(_.longitude)) })
  /**
   * Add new photo.
   * Brand new id will be generated and injected into new Photo instance.
   */
  def addNew(thePath: String, theGeoinfo: Option[GeoInfo] = None, theTimestamp: Option[Timestamp] = None): Photo = {
    val now = DB.now
    val newId = DB withSession {
      def p = createdAt ~ path ~ timestamp.? ~ latitude.? ~ longitude.?
      p returning id insert (now, thePath, theTimestamp, theGeoinfo.map(_.latitude), theGeoinfo.map(_.longitude))
    }
    Photo(newId, now, None, thePath, theTimestamp, theGeoinfo)
  }
  /**
   * Find specified user's all photo
   */
  def findByOwner(owner: User): List[Photo] = {
    DB withSession {
      val q = for {
        o <- PhotoOwner
        if o.userId === owner.id
        photo <- o.photo
      } yield photo
      q.list
    }
  }
  /**
   * Find photo which has given id
   */
  def getById(givenId: Long): Option[Photo] = {
    DB withSession {
      val q = for {
        o <- Photo
        if o.id === givenId
      } yield o
      q.firstOption
    }
  }
}
