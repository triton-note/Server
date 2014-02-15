package models.db

import java.sql.Timestamp
import simple._
import scalaz._
import Scalaz._

case class User(id: Long,
                createdAt: Timestamp = currentTimestamp,
                lastModifiedAt: Option[Timestamp] = None,
                firstName: String,
                lastName: String,
                avatarUrl: Option[String] = None) {
  /**
   * Query for me
   */
  private def me = Users.filter(_.id is id)
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[User] = DB withSession { implicit session => me.firstOption }
  /**
   * Delete me
   */
  def delete: Boolean = DB withSession { implicit session =>
    me.delete > 0
  }
  /**
   * Combination of firstName and lastName
   */
  lazy val fullName: String = f"$firstName $lastName"
  /**
   * List of user aliases in any domain, sorted by priority
   */
  lazy val aliases: List[UserAlias] = DB withSession { implicit session =>
    UserAliases.filter(_.userId is id).sortBy(_.domain).sortBy(_.priority).list
  }
  /**
   * List of email addresses, sorted by priority
   */
  lazy val emails: List[String] = aliases.filter(_.domain == UserAliasDomain.email).map(_.name)
  /**
   * Change properties (like a copy) and update Database
   */
  def update(firstName: String = this.firstName, lastName: String = this.lastName, avatarUrl: Option[String] = this.avatarUrl): Option[User] = {
    val n = copy(lastModifiedAt = Some(currentTimestamp), firstName = firstName, lastName = lastName, avatarUrl = avatarUrl)
    DB withSession { implicit session =>
      me.map { a =>
        (a.lastModifiedAt.?, a.firstName, a.lastName, a.avatarUrl.?)
      }.update((n.lastModifiedAt, n.firstName, n.lastName, n.avatarUrl)) == 1
    } option n
  }
}
class Users(tag: Tag) extends Table[User](tag, "USER") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def firstName = column[String]("FIRST_NAME", O.NotNull)
  def lastName = column[String]("LAST_NAME", O.NotNull)
  def avatarUrl = column[String]("AVATAR_URL", O.Nullable)
  // All columns
  def * = (id, createdAt, lastModifiedAt.?, firstName, lastName, avatarUrl.?) <> (User.tupled, User.unapply)
}
object Users extends TableQuery(new Users(_)) {
  /**
   * Add new user
   */
  def addNew(firstName: String, lastName: String, avatarUrl: Option[String] = None): Option[User] = {
    val obj = User(id = -1, firstName = firstName, lastName = lastName, avatarUrl = avatarUrl)
    val newId = DB withSession { implicit session =>
      (this returning map(_.id)) += obj
    }
    Option(newId) map { id => obj.copy(id = id) }
  }
}