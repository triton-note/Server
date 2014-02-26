package models.db

import java.util.Date
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import com.amazonaws.services.dynamodbv2.model._

case class Geographic(id: Long,
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
  def update(equatorialRadius: Double = equatorialRadius, polarRadius: Double = polarRadius): Option[Geographic] = Geographics.update(id,
    Geographics.equatorialRadius(equatorialRadius),
    Geographics.polarRadius(polarRadius)
  )
}

object Geographics extends AutoIDTable[Geographic]("GEOGRAPHIC") {
  val equatorialRadius = Column[Double]("EQUATORIAL_RADIUS", (_.equatorialRadius), (_.getDouble), attrDouble) // in meter
  val polarRadius = Column[Double]("POLAR_RADIUS", (_.polarRadius), (_.getDouble), attrDouble) // in meter
  // All columns
  val columns = Set(equatorialRadius, polarRadius)
  /**
   * Add new
   */
  def addNew(theEquatorialRadius: Double, thePolarRadius: Double): Option[Geographic] = addNew(
    equatorialRadius(theEquatorialRadius),
    polarRadius(thePolarRadius)
  )
}
