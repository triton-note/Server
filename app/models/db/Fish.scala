package models.db

import java.util.Date

import scalaz.Scalaz._

import org.fathens.play.util.Exception.allCatch

import com.amazonaws.services.dynamodbv2.model._

case class FishSize(id: Long,
  createdAt: Date,
  lastModifiedAt: Option[Date],
  photo: Option[Photo],
  name: String,
  count: Long,
  weight: Option[(Double, String)],
  length: Option[(Double, String)]) {
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[FishSize] = FishSizes.get(id)
  /**
   * Delete me
   */
  def delete: Boolean = FishSizes.delete(id)
}
object FishSizes extends AutoIDTable[FishSize]("FISH_SIZE") {
  val photo = Column[Option[Photo]]("PHOTO", (_.photo), (_.get(Photos)), attrObjLongID)
  val name = Column[String]("NAME", (_.name), (_.getS), attrString)
  val count = Column[Long]("COUNT", (_.count), (_.getLong), attrLong)
  val weight = Column[Option[Double]]("WEIGHT", (_.weight.map(_._1)), (_.getDouble.some), attrDouble)
  val weightUnit = Column[Option[String]]("WEIGHT_UNIT", (_.weight.map(_._2)), (_.getS.some), attrString)
  val length = Column[Option[Double]]("LENGTH", (_.length.map(_._1)), (_.getDouble.some), attrDouble)
  val lengthUnit = Column[Option[String]]("LENGTH_UNIT", (_.length.map(_._2)), (_.getS.some), attrString)
  // All columns
  val columns = List(photo, name, weight, length)
  def fromMap(implicit map: Map[String, AttributeValue]): Option[FishSize] = allCatch opt FishSize(
    id.build,
    createdAt.build,
    lastModifiedAt.build,
    photo.build,
    name.build,
    count.build,
    for { v <- weight.build; u <- weightUnit.build } yield (v, u),
    for { v <- length.build; u <- lengthUnit.build } yield (v, u)
  )
  /**
   * Add new fish size
   */
  def addNew(thePhoto: Photo, theName: String, theCount: Long,
    theWeight: Option[(Double, String)] = None, theLength: Option[(Double, String)] = None): Option[FishSize] = addNew(
    photo(Option(thePhoto)),
    name(theName),
    count(theCount),
    weight(theWeight.map(_._1)),
    weightUnit(theWeight.map(_._2)),
    length(theLength.map(_._1)),
    lengthUnit(theLength.map(_._2))
  )
}

