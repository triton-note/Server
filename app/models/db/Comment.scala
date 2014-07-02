package models.db

import java.util.Date

import scala.util.control.Exception.allCatch

import com.amazonaws.services.dynamodbv2.model._

case class Comment(id: String,
                   createdAt: Date,
                   lastModifiedAt: Option[Date],
                   user: Option[User],
                   catchReport: Option[CatchReport],
                   text: String) {
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[Comment] = Comments.get(id)
  /**
   * Delete me
   */
  def delete: Boolean = Comments.delete(id)
  /**
   * Change text
   */
  def update(text: String): Option[Comment] = {
    Comments.update(id, Map(Comments.text(text)))
  }
}
object Comments extends AutoIDTable[Comment]("COMMENT") {
  val user = Column[Option[User]]("USER", (_.user), (_.get(Users)), attrObjID)
  val catchReport = Column[Option[CatchReport]]("CATCH_REPORT", (_.catchReport), (_.get(CatchReports)), attrObjID)
  val text = Column[String]("TEXT", (_.text), (_.getString getOrElse ""), attrString)
  // All columns
  val columns = List(user, catchReport, text)
  def fromMap(implicit map: Map[String, AttributeValue]): Option[Comment] = allCatch opt Comment(
    id.build,
    createdAt.build,
    lastModifiedAt.build,
    user.build,
    catchReport.build,
    text.build
  )
  /**
   * Add new comment
   */
  def addNew(theUser: User, theCatchReport: CatchReport, theText: String): Comment = addNew(
    user(Option(theUser)),
    catchReport(Option(theCatchReport)),
    text(theText)
  )
}
