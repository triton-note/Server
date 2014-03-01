package models.db

import java.util.Date
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import com.amazonaws.services.dynamodbv2.model._
import scala.concurrent.duration._
import models.Storage

case class Photo(id: Long,
                 createdAt: Date,
                 lastModifiedAt: Option[Date],
                 catchReport: Option[CatchReport],
                 image: Option[Image]) {
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[Photo] = Photos.get(id)
  /**
   * Delete me
   */
  def delete: Boolean = Photos.delete(id)
}
object Photos extends AutoIDTable[Photo]("PHOTO") {
  val catchReport = Column[Option[CatchReport]]("CATCH_REPORT", (_.catchReport), (_.get(CatchReports)), attrObjLongID)
  val image = Column[Option[Image]]("IMAGE", (_.image), (_.get(Images)), attrObjLongID)
  val columns = Set(catchReport, image)
  /**
   * Add new photo.
   * Brand new id will be generated and injected into new Photo instance.
   */
  def addNew(theCatchReport: CatchReport, theImage: Image): Option[Photo] = addNew(
    catchReport(Option(theCatchReport)),
    image(Option(theImage))
  )
}

case class Image(id: Long,
                 createdAt: Date,
                 lastModifiedAt: Option[Date],
                 kind: String,
                 format: String,
                 dataSize: Long,
                 width: Long,
                 height: Long) {
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[Image] = Images.get(id)
  /**
   * Delete me
   */
  def delete: Boolean = Images.delete(id)
  lazy val file = Storage.file("photo", kind, id.toString)
  def url(implicit limit: FiniteDuration = 1 minute) = file.generateURL(limit)
}
object Images extends AutoIDTable[Image]("IMAGE") {
  val kind = Column[String]("KIND", (_.kind), (_.getS), attrString)
  val format = Column[String]("FORMAT", (_.format), (_.getS), attrString)
  val dataSize = Column[Long]("DATA_SIZE", (_.dataSize), (_.getLong), attrLong)
  val width = Column[Long]("WIDTH", (_.width), (_.getLong), attrLong)
  val height = Column[Long]("HEIGHT", (_.height), (_.getLong), attrLong)
  // All columns
  val columns = Set(kind, format, dataSize, width, height)
  /**
   * Add new image data
   */
  def addNew(theDataSize: Long, theWidth: Long, theHeight: Long,
             theFormat: String = "JPEG", theKind: String = KIND_ORIGINAL): Option[Image] = addNew(
    kind(theKind),
    format(theFormat),
    dataSize(theDataSize),
    width(theWidth),
    height(theHeight)
  )
  val KIND_ORIGINAL = "original"
}

case class ImageRelation(id: Long,
                         createdAt: Date,
                         lastModifiedAt: Option[Date],
                         imageSrc: Option[Image],
                         imageDst: Option[Image],
                         relation: String) {
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[ImageRelation] = ImageRelations.get(id)
  /**
   * Delete me
   */
  def delete: Boolean = ImageRelations.delete(id)
}
object ImageRelations extends AutoIDTable[ImageRelation]("IMAGE_RELATION") {
  val imageSrc = Column[Option[Image]]("IMAGE_SRC", (_.imageSrc), (_.get(Images)), attrObjLongID)
  val imageDst = Column[Option[Image]]("IMAGE_DST", (_.imageDst), (_.get(Images)), attrObjLongID)
  val relation = Column[String]("RELATION", (_.relation), (_.getS), attrString)
  // All columns
  val columns = Set(imageSrc, imageDst, relation)
  /**
   * Add new relation
   */
  def addNew(theImageSrc: Image, theImageDst: Image, theRelation: String): Option[ImageRelation] = addNew(
    imageSrc(Option(theImageSrc)),
    imageDst(Option(theImageDst)),
    relation(theRelation)
  )
}
