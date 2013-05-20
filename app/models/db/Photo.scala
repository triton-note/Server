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
                 timestamp: Option[Timestamp],
                 geoinfo: Option[GeoInfo]) {
  lazy val comments = withSession {
    val q = for {
      a <- me
      b <- CommentPhoto
      if (b.photoId === a.id)
      c <- b.comment
    } yield c
    q.sortBy(_.lastModifiedAt).list
  }
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
    withSession {
      me.delete
    }
  }
  /**
   * Change property (like a copy) and update Database
   */
  def update(theTimestamp: Option[Timestamp] = timestamp, theGeoinfo: Option[GeoInfo] = geoinfo): Photo = {
    val n = copy(lastModifiedAt = Some(currentTimestamp), timestamp = theTimestamp, geoinfo = theGeoinfo)
    withSession {
      me.map { a =>
        (a.lastModifiedAt.? ~ a.timestamp.? ~ a.latitude.? ~ a.longitude.?)
      }.update((n.lastModifiedAt, n.timestamp, n.geoinfo.map(_.latitude), n.geoinfo.map(_.longitude)))
    }
    n
  }
  def bindTo(user: User) = withSession {
    PhotoOwner.addNew(this, user)._1
  }
  def bindTo(album: Album) = withSession {
    PhotoAlbum.addNew(this, album)._1
  }
  /**
   * Add comment
   */
  def add(text: String)(implicit user: User) = withSession {
    val comment = Comment.addNew(user, text)
    CommentPhoto.addNew(comment, this)._2
  }
}

object Photo extends Table[Photo]("PHOTO") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def timestamp = column[Timestamp]("TIMESTAMP", O.Nullable)
  def latitude = column[Double]("LATITUDE", O.Nullable)
  def longitude = column[Double]("LONGITUDE", O.Nullable)
  // All columns
  def * = id ~ createdAt ~ lastModifiedAt.? ~ timestamp.? ~ latitude.? ~ longitude.? <> (
    { t => Photo(t._1, t._2, t._3, t._4, GeoInfo(t._5, t._6)) },
    { o: Photo => Some(o.id, o.createdAt, o.lastModifiedAt, o.timestamp, o.geoinfo.map(_.latitude), o.geoinfo.map(_.longitude)) })
  /**
   * Add new photo.
   * Brand new id will be generated and injected into new Photo instance.
   */
  def addNew(theGeoinfo: Option[GeoInfo] = None, theTimestamp: Option[Timestamp] = None): Photo = {
    val now = currentTimestamp
    val newId = withSession {
      def p = createdAt ~ timestamp.? ~ latitude.? ~ longitude.?
      p returning id insert (now, theTimestamp, theGeoinfo.map(_.latitude), theGeoinfo.map(_.longitude))
    }
    Photo(newId, now, None, theTimestamp, theGeoinfo)
  }
  /**
   * Find specified user's all photo
   */
  def findByOwner(owner: User): List[Photo] = {
    withSession {
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
    withSession {
      val q = for {
        o <- Photo
        if o.id === givenId
      } yield o
      q.firstOption
    }
  }
}

case class PhotoData(path: String,
                     folder: String,
                     infoId: Long,
                     createdAt: Timestamp,
                     lastModifiedAt: Option[Timestamp],
                     format: String,
                     filesize: Long,
                     width: Long,
                     height: Long) {
  lazy val file = Storage.file("photo", folder, path)
  def url(implicit limit: FiniteDuration = 1 minute) = file.generateURL(limit)
  /**
   * Prepared query for me
   */
  lazy val me = for {
    a <- PhotoData
    if (a.path === path)
  } yield a
}

object PhotoData extends Table[PhotoData]("PHOTO_DATA") {
  def path = column[String]("PATH", O.PrimaryKey)
  def folder = column[String]("FOLDER", O.NotNull)
  def infoId = column[Long]("INFO_ID", O.NotNull)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def format = column[String]("FORMAT", O.NotNull)
  def filesize = column[Long]("FILESIZE", O.NotNull)
  def width = column[Long]("WIDTH", O.NotNull)
  def height = column[Long]("HEIGHT", O.NotNull)
  // All columns
  def * = path ~ folder ~ infoId ~ createdAt ~ lastModifiedAt.? ~ format ~ filesize ~ width ~ height <> (PhotoData.apply _, PhotoData.unapply _)
  /**
   * Bound photo
   */
  def info = foreignKey("PHOTO_DATA_FK_INFO", infoId, Photo)(_.id)
  /**
   * Index for infoId
   */
  def infoIndex = index("PHOTO_DATA_INDEX_INFO", infoId, true)
  /**
   * Add new photo data
   */
  def addNew(thePath: String, theFolder: String, theInfo: Photo, theFormat: String, theFilesize: Long, theWidth: Long, theHeight: Long): PhotoData = {
    val p = PhotoData(thePath, theFolder, theInfo.id, currentTimestamp, None, theFormat, theFilesize, theWidth, theHeight)
    withSession {
      * insert p
    }
    p
  }
}
