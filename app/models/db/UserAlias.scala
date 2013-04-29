package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession
import securesocial.core.providers.UsernamePasswordProvider
import securesocial.core.providers.FacebookProvider

case class UserAlias(createdAt: Timestamp = currentTimestamp,
                     lastModifiedAt: Option[Timestamp] = None,
                     user: User,
                     name: String,
                     domain: String,
                     priority: Int,
                     password: Option[String] = None,
                     passwordHashing: Option[String] = None) {
  lazy val email = if (domain == UserAliasDomain.email) Some(name) else user.emails.headOption
  /**
   * Prepared query for me
   */
  lazy val me = for {
    o <- UserAlias
    if (o.name === name)
    if (o.domain === domain)
  } yield o
  /**
   * Delete me
   */
  def delete: Boolean = {
    val v = withSession {
      me.delete
    }
    v > 0
  }
  /**
   * Change priority
   */
  def changePriority(thePriority: Int): UserAlias = {
    withSession {
      me.map(_.priority).update(thePriority)
    }
    copy(priority = thePriority)
  }
  /**
   * Change password by hashed password and hasher's id
   */
  def changePassword(thePassword: String, theHashing: Option[String] = None): UserAlias = {
    withSession {
      me.map { o =>
        o.password ~ o.passwordHashing.?
      }.update(thePassword, theHashing)
    }
    copy(password = Some(thePassword), passwordHashing = theHashing)
  }
}
object UserAlias extends Table[UserAlias]("USER_ALIAS") {
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
  def * = createdAt ~ lastModifiedAt.? ~ userId ~ name ~ domain ~ priority ~ password.? ~ passwordHashing.? <> (
    { t => UserAlias(t._1, t._2, User.get(t._3).get, t._4, t._5, t._6, t._7, t._8) },
    { o => Some(o.createdAt, o.lastModifiedAt, o.user.id, o.name, o.domain, o.priority, o.password, o.passwordHashing) })
  /**
   * Bound user
   */
  def user = foreignKey("USER_ALIAS_FK_USER", userId, User)(_.id)
  /**
   * Add new user alias
   */
  def addNew(theUser: User, theName: String, theDomain: String, thePriority: Int,
      thePassword: Option[String] = None, theHashing: Option[String] = None): UserAlias = {
    val o = UserAlias(currentTimestamp, None, theUser, theName, theDomain, thePriority, thePassword, theHashing)
    withSession {
      * insert o
    }
    o
  }
  /**
   * Find a alias
   */
  def get(theName: String, theDomain: String): Option[UserAlias] = withSession {
    val q = for {
      o <- UserAlias
      if (o.name === theName)
      if (o.domain === theDomain)
    } yield o
    q.firstOption
  }
  /**
   * Find by email
   */
  def getByEmail(theEmail: String): Option[UserAlias] = get(theEmail, UserAliasDomain.email)
  /**
   * List by user in specified domain
   */
  def list(theUser: User, theDomain: String): List[UserAlias] = withSession {
    val q = for {
      o <- UserAlias
      if (o.userId === theUser.id)
      if (o.domain === theDomain)
    } yield o
    q.sortBy(_.priority).list
  }
  /**
   * List by user in any domain
   */
  def list(theUser: User): List[UserAlias] = withSession {
    val q = for {
      o <- UserAlias
      if (o.userId === theUser.id)
    } yield o
    q.sortBy(_.domain).sortBy(_.priority).list
  }
}

object UserAliasDomain {
  import securesocial.core._
  val email = UsernamePasswordProvider.UsernamePassword
  val facebook = FacebookProvider.Facebook
}
