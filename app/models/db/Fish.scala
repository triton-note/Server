package models.db

import scala.collection.JavaConversions._

import com.amazonaws.services.dynamodbv2.model._

import models.Report.Fishes.SizeValue
import models.ValueUnit.{ Length, Weight }

case class FishSize(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[FishSize] {
  val TABLE = FishSize

  lazy val photo: Option[Photo] = build(_.photo)
  lazy val name: String = build(_.name)
  lazy val count: Long = build(_.count)
  lazy val size: SizeValue = build(_.size)
}
object FishSize extends AutoIDTable[FishSize]("FISH_SIZE") {
  val photo = Column[Option[Photo]]("PHOTO", (_.photo), (_.get(Photo)), attrObjID)
  val name = Column[String]("NAME", (_.name), (_.getString.get), attrString)
  val count = Column[Long]("COUNT", (_.count), (_.getLong.get), attrLong)
  val size = Column[SizeValue]("SIZE", (_.size), (_.getJson.get.as[SizeValue]), (_.asJson))
  // All columns
  val columns = List(photo, name, count, size)
  /**
   * Add new fish size
   */
  def addNew(thePhoto: Photo, theName: String, theCount: Long,
    theWeight: Option[Weight] = None, theLength: Option[Length] = None): FishSize = addNew(
    photo(Option(thePhoto)),
    name(theName),
    count(theCount),
    size(SizeValue(theWeight, theLength))
  )
  def findBy(theName: String): List[FishSize] = {
    find(_.withIndexName("NAME-CREATED_AT-index").withKeyConditions(Map(
      name compare theName
    ))).toList
  }
  def findBy(thePhoto: Photo): List[FishSize] = {
    find(_.withIndexName("PHOTO-CREATED_AT-index").withKeyConditions(Map(
      photo compare Option(thePhoto)
    ))).toList
  }
}
