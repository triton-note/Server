package models.db

import java.sql.Timestamp
import simple._
import scalaz._
import Scalaz._

case class Comment(id: Long,
                   createdAt: Timestamp = currentTimestamp,
                   lastModifiedAt: Option[Timestamp] = None,
                   userId: Long,
                   catchId: Long,
                   text: String) {
  /**
   * Query for me
   */
  private def me = Comments.filter(_.id is id)
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[Comment] = DB withSession { implicit session => me.firstOption }
  /**
   * Delete me
   */
  def delete: Boolean = DB withSession { implicit session =>
    me.delete > 0
  }
  /**
   * Owner of me
   */
  lazy val user: Option[User] = DB withSession { implicit session =>
    me.flatMap(_.user).firstOption
  }
  /**
   * CatchReport
   */
  lazy val catchReport: Option[CatchReport] = DB withSession { implicit session =>
    me.flatMap(_.catchReport).firstOption
  }
  /**
   * Change text
   */
  def update(text: String): Option[Comment] = {
    val n = copy(lastModifiedAt = Some(currentTimestamp), text = text)
    DB withSession { implicit session =>
      me.map { a =>
        (a.lastModifiedAt.?, a.text)
      }.update((n.lastModifiedAt, n.text)) == 1
    } option n
  }
}

class Comments(tag: Tag) extends Table[Comment](tag, "COMMENT") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def userId = column[Long]("USER", O.NotNull)
  def catchReportId = column[Long]("CATCH_REPORT", O.NotNull)
  def text = column[String]("TEXT", O.NotNull, O.Default(""))
  // All columns
  def * = (id, createdAt, lastModifiedAt.?, userId, catchReportId, text) <> (Comment.tupled, Comment.unapply)
  /**
   * Bound user
   */
  def user = foreignKey("COMMENT_FK_USER", userId, Users)(_.id)
  /**
   * Bound catch
   */
  def catchReport = foreignKey("COMMENT_FK_CATCH_REPORT", catchReportId, CatchReports)(_.id)
}
object Comments extends TableQuery(new Comments(_)) {
  /**
   * Add new comment
   */
  def addNew(user: User, catchReport: CatchReport, text: String): Option[Comment] = {
    val obj = Comment(id = -1, userId = user.id, catchId = catchReport.id, text = text)
    val newId = DB withSession { implicit session =>
      (this returning map(_.id)) += obj
    }
    Option(newId) map { id => obj.copy(id = id) }
  }
}
