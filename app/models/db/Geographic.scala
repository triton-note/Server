package models.db

import com.amazonaws.services.dynamodbv2.model._

case class Geographic(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[Geographic] {
  val TABLE = Geographic

  lazy val equatorialRadius: Double = build(_.equatorialRadius)
  lazy val polarRadius: Double = build(_.polarRadius)
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
  /**
   * Add new
   */
  def addNew(name: String, theEquatorialRadius: Double, thePolarRadius: Double): Geographic = addNew(name,
    equatorialRadius(theEquatorialRadius),
    polarRadius(thePolarRadius)
  )
  def get: Geographic = get("Earth").get
}
