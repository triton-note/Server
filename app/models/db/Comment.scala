package models.db

import scala.collection.JavaConversions._

import com.amazonaws.services.dynamodbv2.model._

case class Comment(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[Comment] {
  val TABLE = Comment

  lazy val user: Option[User] = build(_.user)
  lazy val catchReport: Option[CatchReport] = build(_.catchReport)
  lazy val text: String = build(_.text)
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
  /**
   * Add new comment
   */
  def addNew(theUser: User, theCatchReport: CatchReport, theText: String): Comment = addNew(
    user(Option(theUser)),
    catchReport(Option(theCatchReport)),
    text(theText)
  )
  def findBy(theUser: User): List[Comment] = {
    find(_.withIndexName("USER-CREATED_AT-index").withKeyConditions(Map(
      user compare Option(theUser)
    ))).toList
  }
  def findBy(theCatchReport: CatchReport): List[Comment] = {
    find(_.withIndexName("CATCH_REPORT-CREATED_AT-index").withKeyConditions(Map(
      catchReport compare Option(theCatchReport)
    ))).toList
  }
}
