package models.db

import java.util.Date
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import com.amazonaws.services.dynamodbv2.model._

case class User(id: Long,
                createdAt: Date = currentTimestamp,
                lastModifiedAt: Option[Date] = None,
                firstName: String,
                lastName: String,
                avatarUrl: Option[String] = None) {
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[User] = Users.get(id)
  /**
   * Delete me
   */
  def delete: Boolean = Users.delete(id)
  /**
   * Combination of firstName and lastName
   */
  lazy val fullName: String = f"$firstName $lastName"
  /**
   * List of user aliases in any domain, sorted by priority
   */
  lazy val aliases: List[UserAlias] = ???
  /**
   * List of email addresses, sorted by priority
   */
  lazy val emails: List[String] = aliases.filter(_.domain == UserAliasDomain.email).map(_.name)
  /**
   * Change properties (like a copy) and update Database
   */
  def update(firstName: String = this.firstName, lastName: String = this.lastName, avatarUrl: Option[String] = this.avatarUrl): Option[User] = {
    val n = copy(lastModifiedAt = Some(currentTimestamp), firstName = firstName, lastName = lastName, avatarUrl = avatarUrl)
    Users.update(n)
  }
}
object Users extends Table[User]("USER") {
  val firstName = Column[String]("FIRST_NAME", (_.firstName), (_.getS), attrString)
  val lastName = Column[String]("LAST_NAME", (_.lastName), (_.getS), attrString)
  val avatarUrl = Column[Option[String]]("AVATAR_URL", (_.avatarUrl), (_.getS.option), attrStringOpt)
  def columns = Set(firstName, lastName, avatarUrl)
  def fromMap(implicit map: Map[String, AttributeValue]): Option[User] = allCatch opt User(
    id.build,
    createdAt.build,
    lastModifiedAt.build,
    firstName.build,
    lastName.build,
    avatarUrl.build
  )
  /**
   * Add new user
   */
  def addNew(theFirstName: String, theLastName: String, theAvatarUrl: Option[String] = None): Option[User] = addNew(
    firstName(theFirstName),
    lastName(theLastName),
    avatarUrl(theAvatarUrl)
  )
}