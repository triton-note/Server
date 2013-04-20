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
                 geoinfo: Option[GeoInfo],
                 desc: String) {
  import scala.concurrent.duration._
  def url(implicit limit: FiniteDuration = 1 minute) = Storage.file(path).generateURL(limit)
  /**
   * Prepared query for me
   */
  val me = for {
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
  def update(path: String = path, timestamp: Option[Timestamp] = timestamp, geoinfo: Option[GeoInfo] = geoinfo, desc: String = desc): Photo = {
    val n = copy(lastModifiedAt = Some(DB.now), path = path, timestamp = timestamp, geoinfo = geoinfo, desc = desc)
    DB.withSession {
      me.map { a =>
        (a.lastModifiedAt.? ~ a.path ~ a.timestamp.? ~ a.latitude.? ~ a.longitude.? ~ a.desc)
      }.update(n.lastModifiedAt, n.path, n.timestamp, n.geoinfo.map(_.latitude), n.geoinfo.map(_.longitude), n.desc)
    }
    n
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
  def desc = column[String]("DESCRIPTION", O.NotNull, O.Default(""))
  // All columns
  def * = id ~ createdAt ~ lastModifiedAt.? ~ path ~ timestamp.? ~ latitude.? ~ longitude.? ~ desc <> (
    { t => Photo(t._1, t._2, t._3, t._4, t._5, GeoInfo(t._6, t._7), t._8) },
    { o: Photo => Some(o.id, o.createdAt, o.lastModifiedAt, o.path, o.timestamp, o.geoinfo.map(_.latitude), o.geoinfo.map(_.longitude), o.desc) })
  /**
   * Add new photo.
   * Brand new id will be generated and injected into new Photo instance.
   */
  def addNew(thePath: String, theDescription: String, theGeoinfo: Option[GeoInfo] = None, theTimestamp: Option[Timestamp] = None): Photo = {
    val now = DB.now
    val newId = DB withSession {
      def p = createdAt ~ path ~ timestamp.? ~ latitude.? ~ longitude.? ~ desc
      p returning id insert (now, thePath, theTimestamp, theGeoinfo.map(_.latitude), theGeoinfo.map(_.longitude), theDescription)
    }
    Photo(newId, now, None, thePath, theTimestamp, theGeoinfo, theDescription)
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
