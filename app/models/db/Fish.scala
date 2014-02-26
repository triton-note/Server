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
  val weight = Column[Option[Double]]("WEIGHT", (_.weight), (_.getDouble.option), attrDouble)
  val length = Column[Option[Double]]("LENGTH", (_.length), (_.getDouble.option), attrDouble)
  // All columns
  val columns = Set(photo, name, weight, length)
  /**
   * Add new fish size
   */
  def addNew(thePhoto: Photo, theName: String, theWeight: Option[Double] = None, theLength: Option[Double] = None): Option[FishSize] = addNew(
    photo(Option(thePhoto)),
    name(theName),
    weight(theWeight),
    length(theLength)
  )
}

