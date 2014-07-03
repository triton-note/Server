package models.db

import java.util.Date

import scala.util.control.Exception.allCatch

import com.amazonaws.services.dynamodbv2.model._

case class Geographic(id: String,
  createdAt: Date,
  lastModifiedAt: Option[Date],
  equatorialRadius: Double,
  polarRadius: Double) extends TimestampedTable.ObjType[Geographic] {
  val TABLE = Geographic
  /**
   * Change property (like a copy) and update Database
   */
  def update(equatorialRadius: Double = equatorialRadius, polarRadius: Double = polarRadius): Option[Geographic] = Geographic.update(id, Map(
    Geographic.equatorialRadius(equatorialRadius),
    Geographic.polarRadius(polarRadius)
  ))
}

object Geographic extends AnyIDTable[Geographic]("GEOGRAPHIC") {
  val equatorialRadius = Column[Double]("EQUATORIAL_RADIUS", (_.equatorialRadius), (_.getDouble.get), attrDouble) // in meter
  val polarRadius = Column[Double]("POLAR_RADIUS", (_.polarRadius), (_.getDouble.get), attrDouble) // in meter
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
  def addNew(name: String, theEquatorialRadius: Double, thePolarRadius: Double): Geographic = addNew(name,
    equatorialRadius(theEquatorialRadius),
    polarRadius(thePolarRadius)
  )
  def get: Geographic = get("Earth").get
}
