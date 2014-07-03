package models.db

import javax.imageio.ImageIO

import scala.collection.JavaConversions._
import scala.concurrent.duration._

import org.fathens.play.util.Exception.allCatch

import com.amazonaws.services.dynamodbv2.model._

import models.Storage

case class Photo(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[Photo] {
  val TABLE = Photo

  lazy val catchReport: Option[CatchReport] = build(_.catchReport)
  lazy val image: Option[Image] = build(_.image)
}
object Photo extends AutoIDTable[Photo]("PHOTO") {
  val catchReport = Column[Option[CatchReport]]("CATCH_REPORT", (_.catchReport), (_.get(CatchReport)), attrObjID)
  val image = Column[Option[Image]]("IMAGE", (_.image), (_.get(Image)), attrObjID)
  // All columns
  val columns = List(catchReport, image)
  /**
   * Add new photo.
   * Brand new id will be generated and injected into new Photo instance.
   */
  def addNew(theCatchReport: CatchReport, theImage: Image): Photo = addNew(
    catchReport(Option(theCatchReport)),
    image(Option(theImage))
  )
  def findBy(cr: CatchReport): List[Photo] = {
    find(_.withIndexName("CATCH_REPORT-index").withKeyConditions(Map(
      catchReport compare Option(cr)
    ))).toList
  }
}

case class Image(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[Image] {
  val TABLE = Image

  lazy val kind: String = build(_.kind)
  lazy val format: String = build(_.format)
  lazy val dataSize: Long = build(_.dataSize)
  lazy val width: Long = build(_.width)
  lazy val height: Long = build(_.height)

  override def delete: Boolean = file.delete && super.delete
  /**
   * Reference to Image file
   */
  lazy val file = Storage.file("photo", kind, id.toString)
  def url(implicit limit: FiniteDuration = 1 hour) = file.generateURL(limit)
}
object Image extends AutoIDTable[Image]("IMAGE") {
  val kind = Column[String]("KIND", (_.kind), (_.getString.get), attrString)
  val format = Column[String]("FORMAT", (_.format), (_.getString.get), attrString)
  val dataSize = Column[Long]("DATA_SIZE", (_.dataSize), (_.getLong.get), attrLong)
  val width = Column[Long]("WIDTH", (_.width), (_.getLong.get), attrLong)
  val height = Column[Long]("HEIGHT", (_.height), (_.getLong.get), attrLong)
  // All columns
  val columns = List(kind, format, dataSize, width, height)
  /**
   * Add new image data
   */
  def addNew(imageFile: java.io.File): Option[Image] = {
    for {
      biOpt <- allCatch opt ImageIO.read(imageFile)
      bi <- Option(biOpt)
      image = addNew(imageFile.length, bi.getWidth, bi.getHeight)
    } yield {
      image.file write imageFile
      image
    }
  }
  def addNew(theDataSize: Long, theWidth: Long, theHeight: Long,
    theFormat: String = "JPEG", theKind: String = KIND_ORIGINAL): Image = addNew(
    kind(theKind),
    format(theFormat),
    dataSize(theDataSize),
    width(theWidth),
    height(theHeight)
  )
  def findBy(theKind: String): List[Image] = {
    find(_.withIndexName("KIND-index").withKeyConditions(Map(
      kind compare theKind
    ))).toList
  }
  val KIND_ORIGINAL = "original"
}

case class ImageRelation(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[ImageRelation] {
  val TABLE = ImageRelation

  lazy val imageSrc: Option[Image] = build(_.imageSrc)
  lazy val imageDst: Option[Image] = build(_.imageDst)
  lazy val relation: String = build(_.relation)
}
object ImageRelation extends AutoIDTable[ImageRelation]("IMAGE_RELATION") {
  val imageSrc = Column[Option[Image]]("IMAGE_SRC", (_.imageSrc), (_.get(Image)), attrObjID)
  val imageDst = Column[Option[Image]]("IMAGE_DST", (_.imageDst), (_.get(Image)), attrObjID)
  val relation = Column[String]("RELATION", (_.relation), (_.getString.get), attrString)
  // All columns
  val columns = List(imageSrc, imageDst, relation)
  /**
   * Add new relation
   */
  def addNew(theImageSrc: Image, theImageDst: Image, theRelation: String): ImageRelation = addNew(
    imageSrc(Option(theImageSrc)),
    imageDst(Option(theImageDst)),
    relation(theRelation)
  )
  def findBy(theSrc: Image, theRelation: String): List[ImageRelation] = {
    find(_.withIndexName("IMAGE_SRC-RELATION-index").withKeyConditions(Map(
      imageSrc compare Option(theSrc),
      relation compare theRelation
    ))).toList
  }
  def findBy(theSrc: Image, theDst: Image): List[ImageRelation] = {
    find(_.withIndexName("IMAGE_SRC-IMAGE_DST-index").withKeyConditions(Map(
      imageSrc compare Option(theSrc),
      imageDst compare Option(theDst)
    ))).toList
  }
}
