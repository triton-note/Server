package models.db

import java.sql.Timestamp
import simple._
import scalaz._
import Scalaz._
import models.GeoInfo
import models.Storage
import scala.concurrent.duration._

case class Photo(catchReportId: Long, imageId: Long) {
  /**
   * Query for me
   */
  private def me = Photos.filter(o => (o.catchReportId is catchReportId) && (o.imageId is imageId))
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[Photo] = DB withSession { implicit session => me.firstOption }
  /**
   * Delete me
   */
  def delete: Boolean = DB withSession { implicit session =>
    me.delete > 0
  }
  /**
   * CatchReport
   */
  lazy val catchReport: Option[CatchReport] = DB withSession { implicit session =>
    me.flatMap(_.catchReport).firstOption
  }
  /**
   * Image
   */
  lazy val image: Option[Image] = DB withSession { implicit session =>
    me.flatMap(_.image).firstOption
  }
}
class Photos(tag: Tag) extends Table[Photo](tag, "PHOTO") {
  def catchReportId = column[Long]("CATCH_REPORT", O.NotNull)
  def imageId = column[Long]("IMAGE", O.NotNull)
  def pk = primaryKey("PHOTO_PK", (catchReportId, imageId))
  // All columns
  def * = (catchReportId, imageId) <> (Photo.tupled, Photo.unapply)
  /**
   * Bound catchReport
   */
  def catchReport = foreignKey("PHOTO_FK_CATCH_REPORT", catchReportId, CatchReports)(_.id)
  /**
   * Bound image
   */
  def image = foreignKey("PHOTO_FK_IMAGE", imageId, Images)(_.id)
}
object Photos extends TableQuery(new Photos(_)) {
  /**
   * Add new photo.
   * Brand new id will be generated and injected into new Photo instance.
   */
  def addNew(catchReport: CatchReport, image: Image): Option[Photo] = {
    val obj = Photo(catchReport.id, image.id)
    DB withSession { implicit session =>
      (this += obj) == 1
    } option obj
  }
}

case class Image(id: Long,
                 kind: String,
                 createdAt: Timestamp,
                 format: String,
                 dataSize: Long,
                 width: Long,
                 height: Long) {
  /**
   * Query for me
   */
  private def me = Images.filter(_.id is id)
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[Image] = DB withSession { implicit session => me.firstOption }
  /**
   * Delete me
   */
  def delete: Boolean = {
    file.delete && DB.withSession { implicit session =>
      me.delete > 0
    }
  }
  lazy val file = Storage.file("photo", kind, id.toString)
  def url(implicit limit: FiniteDuration = 1 minute) = file.generateURL(limit)
}
class Images(tag: Tag) extends Table[Image](tag, "IMAGE") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def kind = column[String]("KIND", O.NotNull)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def format = column[String]("FORMAT", O.NotNull)
  def dataSize = column[Long]("DATA_SIZE", O.NotNull)
  def width = column[Long]("WIDTH", O.NotNull)
  def height = column[Long]("HEIGHT", O.NotNull)
  // All columns
  def * = (id, kind, createdAt, format, dataSize, width, height) <> (Image.tupled, Image.unapply)
}
object Images extends TableQuery(new Images(_)) {
  /**
   * Add new image data
   */
  def addNew(kind: String, format: String, dataSize: Long, width: Long, height: Long): Option[Image] = {
    val obj = Image(-1, kind, currentTimestamp, format, dataSize, width, height)
    val newId = DB withSession { implicit session =>
      (this returning map(_.id)) += obj
    }
    Option(newId) map { id => obj.copy(id = id) }
  }
  /**
   * Find image which has given id
   */
  def get(id: Long): Option[Image] = DB withSession { implicit session =>
    filter(_.id is id).firstOption
  }
  val KIND_ORIGINAL = "original"
}

case class ImageRelation(imageSrcId: Long,
                         imageDstId: Long,
                         relation: String) {
  /**
   * Query for me
   */
  private def me = for {
    r <- ImageRelations
    if r.imageSrcId is imageSrcId
    if r.imageDstId is imageDstId
    if r.relation is relation
  } yield r
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[ImageRelation] = DB withSession { implicit session => me.firstOption }
  /**
   * Delete me
   */
  def delete: Boolean = DB withSession { implicit session =>
    me.delete > 0
  }
  /**
   * Image Src
   */
  lazy val imageSrc: Option[Image] = DB withSession { implicit session =>
    me.flatMap(_.imageSrc).firstOption
  }
  /**
   * Image Dst
   */
  lazy val imageDst: Option[Image] = DB withSession { implicit session =>
    me.flatMap(_.imageDst).firstOption
  }
}
class ImageRelations(tag: Tag) extends Table[ImageRelation](tag, "IMAGE_RELATION") {
  def imageSrcId = column[Long]("IMAGE_SRC", O.NotNull)
  def imageDstId = column[Long]("IMAGE_DST", O.NotNull)
  def relation = column[String]("RELATION", O.NotNull)
  // All columns
  def * = (imageSrcId, imageDstId, relation) <> (ImageRelation.tupled, ImageRelation.unapply)
  /**
   * Bound image
   */
  def imageSrc = foreignKey("IMAGE_RELATION_FK_SRC", imageSrcId, Images)(_.id)
  def imageDst = foreignKey("IMAGE_RELATION_FK_DST", imageDstId, Images)(_.id)
}
object ImageRelations extends TableQuery(new ImageRelations(_)) {
  /**
   * Add new relation
   */
  def addNew(imageSrc: Image, imageDst: Image, relation: String): Option[ImageRelation] = {
    val obj = ImageRelation(imageSrc.id, imageDst.id, relation)
    DB withSession { implicit session =>
      (this += obj) == 1
    } option obj
  }
}
