package models.db

import java.sql.Timestamp
import simple._
import scalaz._
import Scalaz._

case class UserAlias(createdAt: Timestamp = currentTimestamp,
                     lastModifiedAt: Option[Timestamp] = None,
                     userId: Long,
                     name: String,
                     domain: String,
                     priority: Int,
                     password: Option[String] = None,
                     passwordHashing: Option[String] = None) {
  /**
   * Query for me
   */
  private def me = UserAliases.filter(o => (o.name is name) && (o.domain is domain))
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[UserAlias] = DB withSession { implicit session => me.firstOption }
  /**
   * Delete me
   */
  def delete: Boolean = DB withSession { implicit session =>
    me.delete > 0
  }
  /**
   * User of me
   */
  lazy val user: Option[User] = DB withSession { implicit session =>
    me.flatMap(_.user).firstOption
  }
  lazy val email: Option[String] = if (domain == UserAliasDomain.email) Some(name) else user.flatMap(_.emails.headOption)
  /**
   * Change priority
   */
  def changePriority(priority: Int): Option[UserAlias] = {
    DB withSession { implicit session =>
      me.map(_.priority).update(priority) == 1
    } option copy(priority = priority)
  }
  /**
   * Change password by hashed password and hasher's id
   */
  def changePassword(password: String, hashing: Option[String] = None): Option[UserAlias] = {
    DB withSession { implicit session =>
      me.map { o =>
        (o.password, o.passwordHashing.?)
      }.update((password, hashing)) == 1
    } option copy(password = Some(password), passwordHashing = hashing)
  }
}
class UserAliases(tag: Tag) extends Table[UserAlias](tag, "USER_ALIAS") {
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def userId = column[Long]("USER", O.NotNull)
  def name = column[String]("NAME", O.NotNull)
  def domain = column[String]("DOMAIN", O.NotNull)
  def password = column[String]("PASSWORD", O.Nullable)
  def passwordHashing = column[String]("PASSWORD_HASHING", O.Nullable)
  def priority = column[Int]("PRIORITY", O.NotNull)
  // Define primary key here
  def pk = primaryKey("USER_ALIAS_PK", (name, domain))
  // All columns
  def * = (createdAt, lastModifiedAt.?, userId, name, domain, priority, password.?, passwordHashing.?) <> (UserAlias.tupled, UserAlias.unapply)
  /**
   * Bound user
   */
  def user = foreignKey("USER_ALIAS_FK_USER", userId, Users)(_.id)
}
object UserAliases extends TableQuery(new UserAliases(_)) {
  /**
   * Add new user alias
   */
  def addNew(user: User,
             name: String,
             domain: String,
             priority: Int,
             password: Option[String] = None,
             passwordHashing: Option[String] = None): Option[UserAlias] = {
    val obj = UserAlias(currentTimestamp, None, user.id, name, domain, priority, password, passwordHashing)
    DB withSession { implicit session =>
      (this += obj) == 1
    } option obj
  }
  /**
   * Find a alias
   */
  def get(name: String, domain: String): Option[UserAlias] = DB withSession { implicit session =>
    filter { o =>
      (o.name is name) && (o.domain is domain)
    }.firstOption
  }
  /**
   * Find by email
   */
  def getByEmail(email: String): Option[UserAlias] = get(email, UserAliasDomain.email)
}

object UserAliasDomain {
  import securesocial.core.providers._
  val email = UsernamePasswordProvider.UsernamePassword
  val facebook = FacebookProvider.Facebook
}
