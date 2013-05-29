package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession
import models.GeoInfo
import models.Storage
import scala.concurrent.duration._

case class Photo(id: Long,
                 createdAt: Timestamp,
                 timestamp: Option[Timestamp],
                 geoinfo: Option[GeoInfo]) {
  lazy val comments = withSession {
    val q = for {
      a <- me
      b <- CommentPhoto
      if b.photoId is a.id
      c <- b.comment
    } yield c
    q.sortBy(_.lastModifiedAt).list
  }
  lazy val albums = withSession {
    val q = for {
      p <- me
      pa <- PhotoAlbum
      if pa.photoId is p.id
      a <- Album
    } yield a
    q.list
  }
  /**
   * Prepared query for me
   */
  lazy val me = for {
    a <- Photo
    if a.id is id
  } yield a
  /**
   * Delete me
   */
  def delete = {
    withSession {
      me.delete
    }
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
  def findAlbum(theGrounds: String, theDate: Timestamp): Option[Album] = withSession {
    val q = for {
      p <- me
      pa <- PhotoAlbum
      if pa.photoId is p.id
      a <- Album
      if a.grounds is theGrounds
      if a.date is theDate
    } yield a
    q.firstOption
  }
}

object Photo extends Table[Photo]("PHOTO") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def timestamp = column[Timestamp]("TIMESTAMP", O.Nullable)
  def latitude = column[Double]("LATITUDE", O.Nullable)
  def longitude = column[Double]("LONGITUDE", O.Nullable)
  // All columns
  def * = id ~ createdAt ~ timestamp.? ~ latitude.? ~ longitude.? <> (
    { t => Photo(t._1, t._2, t._3, GeoInfo(t._4, t._5)) },
    { o: Photo => Some(o.id, o.createdAt, o.timestamp, o.geoinfo.map(_.latitude), o.geoinfo.map(_.longitude)) })
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
    Photo(newId, now, theTimestamp, theGeoinfo)
  }
  /**
   * Find specified user's all photo
   */
  def findByOwner(owner: User): List[Photo] = {
    withSession {
      val q = for {
        o <- PhotoOwner
        if o.userId is owner.id
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
        if o.id is givenId
      } yield o
      q.firstOption
    }
  }
}

case class Image(id: Long,
                 kind: String,
                 infoId: Long,
                 createdAt: Timestamp,
                 format: String,
                 dataSize: Long,
                 width: Long,
                 height: Long) {
  lazy val file = Storage.file("photo", kind, id.toString)
  def url(implicit limit: FiniteDuration = 1 minute) = file.generateURL(limit)
  /**
   * Prepared query for me
   */
  lazy val me = for {
    a <- Image
    if a.id is id
  } yield a
}

object Image extends Table[Image]("PHOTO_DATA") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def kind = column[String]("KIND", O.NotNull)
  def infoId = column[Long]("INFO", O.NotNull)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def format = column[String]("FORMAT", O.NotNull)
  def dataSize = column[Long]("DATA_SIZE", O.NotNull)
  def width = column[Long]("WIDTH", O.NotNull)
  def height = column[Long]("HEIGHT", O.NotNull)
  // All columns
  def * = id ~ kind ~ infoId ~ createdAt ~ format ~ dataSize ~ width ~ height <> (Image.apply _, Image.unapply _)
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
  def addNew(theKind: String, theInfo: Photo, theFormat: String, theDataSize: Long, theWidth: Long, theHeight: Long): Image = {
    val now = currentTimestamp
    val newId = withSession {
      val p = kind ~ infoId ~ createdAt ~ format ~ dataSize ~ width ~ height
      p returning id insert (theKind, theInfo.id, now, theFormat, theDataSize, theWidth, theHeight)
    }
    Image(newId, theKind, theInfo.id, now, theFormat, theDataSize, theWidth, theHeight)
  }
  def get(theId: Long) = withSession {
    val q = for {
      i <- Image
      if i.id is theId
    } yield i
    q.firstOption
  }
}

case class ImageRelation(imageA: Image,
                         imageB: Image,
                         relation: String) {
  /**
   * Prepared query for me
   */
  lazy val me = for {
    r <- ImageRelation
    if r.imageAid is imageA.id
    if r.imageBid is imageB.id
    if r.relation is relation
  } yield r
}
object ImageRelation extends Table[ImageRelation]("IMAGE_RELATION") {
  def imageAid = column[Long]("IMAGE_A", O.NotNull)
  def imageBid = column[Long]("IMAGE_B", O.NotNull)
  def relation = column[String]("RELATION", O.NotNull)
  // All columns
  def * = imageAid ~ imageBid ~ relation <> (
    { t => ImageRelation(Image.get(t._1).get, Image.get(t._2).get, t._3) },
    { o => Some(o.imageA.id, o.imageB.id, o.relation) }
  )
  /**
   * Bound image
   */
  def imageA = foreignKey("IMAGE_RELATION_FK_A", imageAid, Image)(_.id)
  def imageB = foreignKey("IMAGE_RELATION_FK_B", imageBid, Image)(_.id)
  /**
   * Add new relation
   */
  def addNew(theImageA: Image, theImageB: Image, theRelation: String): ImageRelation = {
    val o = ImageRelation(theImageA, theImageB, theRelation)
    withSession {
      * insert o
    }
    o
  }
}
