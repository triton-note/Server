package models.db

import java.util.Date
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import com.amazonaws.services.dynamodbv2.model._

case class FishSize(id: Long,
                    createdAt: Date,
                    lastModifiedAt: Option[Date],
                    photo: Option[Photo],
                    name: String,
                    count: Long,
                    weight: Option[Double],
                    length: Option[Double]) {
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
  val weight = Column[Option[Double]]("WEIGHT", (_.weight), (_.getDouble.some), attrDouble)
  val length = Column[Option[Double]]("LENGTH", (_.length), (_.getDouble.some), attrDouble)
  // All columns
  val columns = List(photo, name, weight, length)
  def fromMap(implicit map: Map[String, AttributeValue]): Option[FishSize] = allCatch opt FishSize(
    id.build,
    createdAt.build,
    lastModifiedAt.build,
    photo.build,
    name.build,
    count.build,
    weight.build,
    length.build
  )
  /**
   * Add new fish size
   */
  def addNew(thePhoto: Photo, theName: String, theCount: Long, theWeight: Option[Double] = None, theLength: Option[Double] = None): Option[FishSize] = addNew(
    photo(Option(thePhoto)),
    name(theName),
    count(theCount),
    weight(theWeight),
    length(theLength)
  )
}

