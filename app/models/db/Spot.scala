package models.db

import scala.collection.JavaConversions._

import com.amazonaws.services.dynamodbv2.model._
import org.fathens.math._

import models.GeoInfo

case class Spot(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[Spot] {
  val TABLE = Spot
  
  lazy val name: String = build(_.name)
  /**
   * Point on map by latitude and longitude
   */
  lazy val geoinfo: GeoInfo = GeoInfo(Degrees(build(_.latitude)), Degrees(build(_.longitude)))
  /**
   * Change name
   */
  def update(name: String): Option[Spot] = {
    TABLE.update(id, Map(TABLE.name(name)))
  }
}
object Spot extends AutoIDTable[Spot]("Spot") {
  val name = Column[String]("NAME", (_.name), (_.getString getOrElse ""), attrString)
  val latitude = Column[Double]("LATITUDE", (_.geoinfo.latitude.value), (_.getDouble.get), attrDouble)
  val longitude = Column[Double]("LONGITUDE", (_.geoinfo.longitude.value), (_.getDouble.get), attrDouble)
  // All columns
  val columns = List(name, latitude, longitude)
}
