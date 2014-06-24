package models.db

import java.util.Date
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import com.amazonaws.services.dynamodbv2.model._

case class Geographic(id: String,
  createdAt: Date,
  lastModifiedAt: Option[Date],
  equatorialRadius: Double,
  polarRadius: Double) {
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[Geographic] = Geographics.get(id)
  /**
   * Delete me
   */
  def delete: Boolean = Geographics.delete(id)
  /**
   * Change property (like a copy) and update Database
   */
  def update(equatorialRadius: Double = equatorialRadius, polarRadius: Double = polarRadius): Option[Geographic] = Geographics.update(id, Map(
    Geographics.equatorialRadius(equatorialRadius),
    Geographics.polarRadius(polarRadius)
  ))
}

object Geographics extends AnyIDTable[Geographic]("GEOGRAPHIC") {
  val equatorialRadius = Column[Double]("EQUATORIAL_RADIUS", (_.equatorialRadius), (_.getDouble), attrDouble) // in meter
  val polarRadius = Column[Double]("POLAR_RADIUS", (_.polarRadius), (_.getDouble), attrDouble) // in meter
  // All columns
  val columns = List(equatorialRadius, polarRadius)
  def fromMap(implicit map: Map[String, AttributeValue]): Option[Geographic] = allCatch opt Geographic(
    id.build,
    createdAt.build,
    lastModifiedAt.build,
    equatorialRadius.build,
    polarRadius.build
  )
  /**
   * Add new
   */
  def addNew(name: String, theEquatorialRadius: Double, thePolarRadius: Double): Option[Geographic] = addNew(name,
    equatorialRadius(theEquatorialRadius),
    polarRadius(thePolarRadius)
  )
  def get: Geographic = get("Earth").get
}
