package models.db

import java.util.Date
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import com.amazonaws.services.dynamodbv2.model._

case class User(id: String,
                createdAt: Date,
                lastModifiedAt: Option[Date],
                password: Option[String],
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
   * Change properties (like a copy) and update Database
   */
  def update(password: Option[String] = this.password,
             firstName: String = this.firstName,
             lastName: String = this.lastName,
             avatarUrl: Option[String] = this.avatarUrl): Option[User] = {
    Users update copy(password = password, firstName = firstName, lastName = lastName, avatarUrl = avatarUrl)
  }
}
object Users extends AnyIDTable[User]("USER") {
  val password = Column[Option[String]]("PASSWORD", (_.avatarUrl), (_.getS.some), attrString)
  val firstName = Column[String]("FIRST_NAME", (_.firstName), (_.getS), attrString)
  val lastName = Column[String]("LAST_NAME", (_.lastName), (_.getS), attrString)
  val avatarUrl = Column[Option[String]]("AVATAR_URL", (_.avatarUrl), (_.getS.some), attrString)
  // All columns
  val columns = Set(firstName, lastName, avatarUrl)
  def fromMap(implicit map: Map[String, AttributeValue]): Option[User] = allCatch opt User(
    id.build,
    createdAt.build,
    lastModifiedAt.build,
    password.build,
    firstName.build,
    lastName.build,
    avatarUrl.build
  )
  /**
   * Add new user
   */
  def addNew(theEmail: String, thePassword: Option[String], theFirstName: String, theLastName: String, theAvatarUrl: Option[String] = None): Option[User] = addNew(theEmail,
    password(thePassword),
    firstName(theFirstName),
    lastName(theLastName),
    avatarUrl(theAvatarUrl)
  )
  def get(email: String): Option[User] = find(id(email)).headOption
  // Password hashing
  val hashingWay = "SHA-1"
  def hash(v: String): String = ???
}