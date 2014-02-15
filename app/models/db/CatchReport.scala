package models.db

import models.GeoInfo
import java.sql.Timestamp
import simple._
import scalaz._
import Scalaz._

case class CatchReport(id: Long,
                 createdAt: Timestamp,
                 timestamp: Option[Timestamp],
                 geoinfo: Option[GeoInfo]) {
  /**
   * Prepared query for me
   */
  private def me = CatchReports.filter(_.id is id)
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[CatchReport] = DB withSession { implicit session => me.firstOption }
  /**
   * Delete me
   */
  def delete = DB withSession { implicit session =>
    me.delete > 0
  }
  lazy val comments: List[Comment] = DB withSession { implicit session =>
    Comments.filter(_.catchReportId is id).sortBy(_.lastModifiedAt).list
  }
  /**
   * Add comment
   */
  def add(text: String)(implicit user: User): Option[Comment] = {
    Comments.addNew(user, this, text)
  }
}

class CatchReports(tag: Tag) extends Table[CatchReport](tag, "CATCH_REPORT") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def timestamp = column[Timestamp]("TIMESTAMP", O.Nullable)
  def latitude = column[Double]("LATITUDE", O.Nullable)
  def longitude = column[Double]("LONGITUDE", O.Nullable)
  // All columns
  def * = (id, createdAt, timestamp.?, latitude.?, longitude.?) <> (
    { t: (Long, Timestamp, Option[Timestamp], Option[Double], Option[Double]) => CatchReport(t._1, t._2, t._3, GeoInfo(t._4, t._5)) },
    { o: CatchReport => Some(o.id, o.createdAt, o.timestamp, o.geoinfo.map(_.latitude), o.geoinfo.map(_.longitude)) })
}
object CatchReports extends TableQuery(new CatchReports(_)) {
  /**
   * Add new
   */
  def addNew(geoinfo: Option[GeoInfo] = None, timestamp: Option[Timestamp] = None): Option[CatchReport] = {
    val obj = CatchReport(-1, currentTimestamp, timestamp, geoinfo)
    val newId = DB withSession { implicit session =>
      (this returning map(_.id)) += obj
    }
    Option(newId) map { id => obj.copy(id = id) }
  }
  /**
   * Find catch which has given id
   */
  def get(id: Long): Option[CatchReport] = DB withSession { implicit session => filter(_.id is id).firstOption }
}
