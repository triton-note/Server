package models.db

import java.util.Date

import scala.util.control.Exception.allCatch

import com.amazonaws.services.dynamodbv2.model._

case class Comment(id: String,
  createdAt: Date,
  lastModifiedAt: Option[Date],
  user: Option[User],
  catchReport: Option[CatchReport],
  text: String) extends TimestampedTable.ObjType[Comment] {
  val TABLE = Comment
  /**
   * Change text
   */
  def update(text: String): Option[Comment] = {
    TABLE.update(id, Map(TABLE.text(text)))
  }
}
object Comment extends AutoIDTable[Comment]("COMMENT") {
  val user = Column[Option[User]]("USER", (_.user), (_.get(User)), attrObjID)
  val catchReport = Column[Option[CatchReport]]("CATCH_REPORT", (_.catchReport), (_.get(CatchReport)), attrObjID)
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
