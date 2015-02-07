package models.db

import java.util.Date

import scala.collection.JavaConversions._

import org.fathens.math._

import com.amazonaws.services.dynamodbv2.model._

import models.{ GeoInfo, Report }

case class CatchReport(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[CatchReport] {
  val TABLE = CatchReport

  lazy val user: Option[User] = build(_.user)
  lazy val timestamp: Date = build(_.timestamp)
  lazy val location: String = build(_.location)
  lazy val condition: Report.Condition = build(_.condition)
  /**
   * Point on map by latitude and longitude
   */
  lazy val geoinfo: GeoInfo = GeoInfo(Degrees(build(_.latitude)), Degrees(build(_.longitude)))
  /**
   * All comments
   */
  lazy val comments: List[Comment] = Comment.findBy(this)
  lazy val topComment: Option[Comment] = user.toList.flatMap(u => comments.filter(_.user == Some(u))).headOption
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
  val condition = Column[Report.Condition]("CONDITION", (_.condition), (_.getJson.get.as[Report.Condition]), (_.asJson))
  val latitude = Column[Double]("LATITUDE", (_.geoinfo.latitude.toDouble), (_.getDouble.get), attrDouble)
  val longitude = Column[Double]("LONGITUDE", (_.geoinfo.longitude.toDouble), (_.getDouble.get), attrDouble)
  // All columns
  val columns = List(user, timestamp, location, condition, latitude, longitude)
  /**
   * Add new
   */
  def addNew(theUser: User, theGeoinfo: GeoInfo, theLocation: String, theTimestamp: Date, theCondition: Report.Condition): CatchReport = addNew(
    user(Some(theUser)),
    timestamp(theTimestamp),
    location(theLocation),
    condition(theCondition),
    latitude(theGeoinfo.latitude.toDouble),
    longitude(theGeoinfo.longitude.toDouble)
  )
  def findBy(theUser: User, count: Int = 0, last: Option[String] = None): List[CatchReport] = {
    find(_.withIndexName("USER-TIMESTAMP-index").withKeyConditions(Map(
      user compare Option(theUser)
    )).withScanIndexForward(false).withLimit(
      count match {
        case 0 => null
        case _ => count
      }).withExclusiveStartKey(
        last.flatMap(get) match {
          case Some(c) => c.toMap(id, user, timestamp)
          case None    => null
        })).toList
  }
  def findBy(theUser: User, theLocation: String): List[CatchReport] = {
    find(_.withIndexName("USER-LOCATION-index").withKeyConditions(Map(
      user compare Option(theUser),
      location compare theLocation
    ))).toList.sortBy(_.timestamp).reverse
  }
}
