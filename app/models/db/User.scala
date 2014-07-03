package models.db

import java.util.Date

import scalaz.Scalaz._

import org.fathens.play.util.Exception.allCatch

import com.amazonaws.services.dynamodbv2.model._

case class User(id: String,
  createdAt: Date,
  lastModifiedAt: Option[Date],
  password: Option[String],
  firstName: String,
  lastName: String,
  avatarUrl: Option[String],
  lengthUnit: String,
  weightUnit: String) extends TimestampedTable.ObjType[User] {
  val TABLE = User
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
    val map = List(
      (password != this.password) option TABLE.password(password),
      (firstName != this.firstName) option TABLE.firstName(firstName),
      (lastName != this.lastName) option TABLE.lastName(lastName),
      (avatarUrl != this.avatarUrl) option TABLE.avatarUrl(avatarUrl)
    ).flatten.toMap
    TABLE.update(id, map)
  }
}
object User extends AnyIDTable[User]("USER") {
  val password = Column[Option[String]]("PASSWORD", (_.password), (_.getString), attrString)
  val firstName = Column[String]("FIRST_NAME", (_.firstName), (_.getString.get), attrString)
  val lastName = Column[String]("LAST_NAME", (_.lastName), (_.getString.get), attrString)
  val avatarUrl = Column[Option[String]]("AVATAR_URL", (_.avatarUrl), (_.getString), attrString)
  val lengthUnit = Column[String]("LENGTH_UNIT", (_.lengthUnit), (_.getString.get), attrString)
  val weightUnit = Column[String]("WEIGHT_UNIT", (_.weightUnit), (_.getString.get), attrString)
  // All columns
  val columns = List(firstName, lastName, avatarUrl)
  def fromMap(implicit map: Map[String, AttributeValue]): Option[User] = allCatch opt User(
    id.build,
    createdAt.build,
    lastModifiedAt.build,
    password.build,
    firstName.build,
    lastName.build,
    avatarUrl.build,
    lengthUnit.build,
    weightUnit.build
  )
  /**
   * Add new user
   */
  def addNew(theEmail: String,
    unhashedPassword: Option[String],
    theFirstName: String,
    theLastName: String,
    theAvatarUrl: Option[String] = None,
    theLengthUnit: String = "cm",
    theWeightUnit: String = "Kg"): User = addNew(theEmail,
    password(unhashedPassword.map(hash)),
    firstName(theFirstName),
    lastName(theLastName),
    avatarUrl(theAvatarUrl),
    lengthUnit(theLengthUnit),
    weightUnit(theWeightUnit)
  )
  def find(email: String): Option[User] = get(email).headOption
  // Password hashing
  val hashingWay = "SHA-1"
  def hash(v: String): String = play.api.libs.Codecs.sha1(v)
}