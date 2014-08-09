package models.db

import scala.collection.JavaConversions._
import scala.concurrent.duration._

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

  lazy val path: String = build(_.path)
  lazy val kind: Image.Kind.Value = TABLE.kind build MAP
  lazy val format: Image.Format.Value = TABLE.format build MAP
  lazy val width: Long = build(_.width)
  lazy val height: Long = build(_.height)

  override def delete: Boolean = file.delete && super.delete
  /**
   * Reference to Image file
   */
  lazy val file = Storage.file(path)
  def url(limit: FiniteDuration) = file.generateURL(limit)
}
object Image extends AutoIDTable[Image]("IMAGE") {
  val path = Column[String]("PATH", (_.path), (_.getString.get), attrString)
  val kind = Column[Kind.Value]("KIND", (_.kind), (Kind withName _.getString.get), attrEnum(Kind))
  val format = Column[Format.Value]("FORMAT", (_.format), (Format withName _.getString.get), attrEnum(Format))
  val width = Column[Long]("WIDTH", (_.width), (_.getLong.get), attrLong)
  val height = Column[Long]("HEIGHT", (_.height), (_.getLong.get), attrLong)
  // All columns
  val columns = List(kind, format, width, height)
  /**
   * Add new image data
   */
  override def putNew(map: Map[String, AttributeValue]): Image = {
    val attributes = if (map contains path.name) map else map + path(
      List("photo", kind.build(map), id.build(map)).mkString("/")
    )
    super.putNew(attributes)
  }
  def addNewWithWriter(writer: java.io.OutputStream => _, thePath: String, theWidth: Long, theHeight: Long, theKind: Kind.Value,
    theFormat: Format.Value = Format.JPEG): Image = {
    val image = addNew(
      path(thePath),
      kind(theKind),
      format(theFormat),
      width(theWidth),
      height(theHeight)
    )
    writer(image.file.newWriter)
    image
  }
  def addNewWithFile(thePath: String, theWidth: Long, theHeight: Long,
    theKind: Kind.Value = Kind.ORIGINAL, theFormat: Format.Value = Format.JPEG): Image = addNew(
    path(thePath),
    kind(theKind),
    format(theFormat),
    width(theWidth),
    height(theHeight)
  )
  def findBy(theKind: Kind.Value): List[Image] = {
    find(_.withIndexName("KIND-index").withKeyConditions(Map(
      kind compare theKind
    ))).toList
  }
  object Format extends Enumeration {
    val JPEG = Value("jpeg")
  }
  object Kind extends Enumeration {
    val ORIGINAL = Value("original")
    val REDUCED = Value("reduced")
  }
}

case class ImageRelation(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[ImageRelation] {
  val TABLE = ImageRelation

  lazy val imageSrc: Option[Image] = build(_.imageSrc)
  lazy val imageDst: Option[Image] = build(_.imageDst)
  lazy val relation: ImageRelation.Relation.Value = TABLE.relation build MAP
}
object ImageRelation extends AutoIDTable[ImageRelation]("IMAGE_RELATION") {
  val imageSrc = Column[Option[Image]]("IMAGE_SRC", (_.imageSrc), (_.get(Image)), attrObjID)
  val imageDst = Column[Option[Image]]("IMAGE_DST", (_.imageDst), (_.get(Image)), attrObjID)
  val relation = Column[Relation.Value]("RELATION", (_.relation), (Relation withName _.getString.get), attrEnum(Relation))
  // All columns
  val columns = List(imageSrc, imageDst, relation)
  /**
   * Add new relation
   */
  def addNew(theImageSrc: Image, theImageDst: Image, theRelation: Relation.Value): ImageRelation = addNew(
    imageSrc(Option(theImageSrc)),
    imageDst(Option(theImageDst)),
    relation(theRelation)
  )
  def findBy(theSrc: Image, theRelation: Relation.Value): List[ImageRelation] = {
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
  object Relation extends Enumeration {
    val THUMBNAIL = Value("thumbnail")
    val MAIN_VIEW = Value("main_view")
  }
}
