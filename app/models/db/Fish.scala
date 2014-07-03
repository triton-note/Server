package models.db

import scala.collection.JavaConversions._

import com.amazonaws.services.dynamodbv2.model._

case class FishSize(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[FishSize] {
  val TABLE = FishSize

  lazy val photo: Option[Photo] = build(_.photo)
  lazy val name: String = build(_.name)
  lazy val count: Long = build(_.count)
  lazy val weight: Option[(Double, String)] = for { value <- build(_.weight); unit <- build(_.weightUnit) } yield (value, unit)
  lazy val length: Option[(Double, String)] = for { value <- build(_.length); unit <- build(_.lengthUnit) } yield (value, unit)
}
object FishSize extends AutoIDTable[FishSize]("FISH_SIZE") {
  val photo = Column[Option[Photo]]("PHOTO", (_.photo), (_.get(Photo)), attrObjID)
  val name = Column[String]("NAME", (_.name), (_.getString.get), attrString)
  val count = Column[Long]("COUNT", (_.count), (_.getLong.get), attrLong)
  val weight = Column[Option[Double]]("WEIGHT", (_.weight.map(_._1)), (_.getDouble), attrDouble)
  val weightUnit = Column[Option[String]]("WEIGHT_UNIT", (_.weight.map(_._2)), (_.getString), attrString)
  val length = Column[Option[Double]]("LENGTH", (_.length.map(_._1)), (_.getDouble), attrDouble)
  val lengthUnit = Column[Option[String]]("LENGTH_UNIT", (_.length.map(_._2)), (_.getString), attrString)
  // All columns
  val columns = List(photo, name, count, weight, weightUnit, length, lengthUnit)
  /**
   * Add new fish size
   */
  def addNew(thePhoto: Photo, theName: String, theCount: Long,
    theWeight: Option[(Double, String)] = None, theLength: Option[(Double, String)] = None): FishSize = addNew(
    photo(Option(thePhoto)),
    name(theName),
    count(theCount),
    weight(theWeight.map(_._1)),
    weightUnit(theWeight.map(_._2)),
    length(theLength.map(_._1)),
    lengthUnit(theLength.map(_._2))
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

