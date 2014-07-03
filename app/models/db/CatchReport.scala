package models.db

import java.util.Date

import scala.collection.JavaConversions._
import scala.math.BigDecimal.double2bigDecimal
import scala.util.control.Exception.allCatch

import com.amazonaws.services.dynamodbv2.model._

import models.GeoInfo

case class CatchReport(id: String,
  createdAt: Date,
  lastModifiedAt: Option[Date],
  user: Option[User],
  timestamp: Date,
  location: String,
  latitude: Double,
  longitude: Double) extends TimestampedTable.ObjType[CatchReport] {
  val TABLE = CatchReport
  /**
   * Point on map by latitude and longitude
   */
  lazy val geoinfo = GeoInfo(latitude, longitude)
  /**
   * All comments
   */
  lazy val comments: List[Comment] = Comment.find(
    _.withIndexName("CATCH_REPORT-CREATED_AT-index").withKeyConditions(Map(
      Comment.catchReport compare Some(this)
    ))).toList
  /**
   * Add comment
   */
  def addComment(text: String)(implicit user: User): Comment = {
    Comment.addNew(user, this, text)
  }
}

object CatchReport extends AutoIDTable[CatchReport]("CATCH_REPORT") {
  val user = Column[Option[User]]("USER", (_.user), (_.get(User)), attrObjID)
  val timestamp = Column[Date]("TIMESTAMP", (_.timestamp), (_.getDate.get), attrDate)
  val location = Column[String]("LOCATION", (_.location), (_.getString.get), attrString)
  val latitude = Column[Double]("LATITUDE", (_.latitude), (_.getDouble.get), attrDouble)
  val longitude = Column[Double]("LONGITUDE", (_.longitude), (_.getDouble.get), attrDouble)
  // All columns
  val columns = List(user, timestamp, location, latitude, longitude)
  def fromMap(implicit map: Map[String, AttributeValue]): Option[CatchReport] = allCatch opt CatchReport(
    id.build,
    createdAt.build,
    lastModifiedAt.build,
    user.build,
    timestamp.build,
    location.build,
    latitude.build,
    longitude.build
  )
  /**
   * Add new
   */
  def addNew(theUser: User, theGeoinfo: GeoInfo, theLocation: String, theTimestamp: Date): CatchReport = addNew(
    user(Some(theUser)),
    timestamp(theTimestamp),
    location(theLocation),
    latitude(theGeoinfo.latitude.toDouble),
    longitude(theGeoinfo.longitude.toDouble)
  )
}
