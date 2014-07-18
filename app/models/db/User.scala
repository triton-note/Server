package models.db

import scalaz.Scalaz._

import com.amazonaws.services.dynamodbv2.model._

case class User(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[User] {
  val TABLE = User

  lazy val email: String = build(_.email)
  lazy val password: Option[String] = build(_.password)
  lazy val firstName: String = build(_.firstName)
  lazy val lastName: String = build(_.lastName)
  lazy val avatarUrl: Option[String] = build(_.avatarUrl)
  lazy val lengthUnit: String = build(_.lengthUnit)
  lazy val weightUnit: String = build(_.weightUnit)
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
    avatarUrl: Option[String] = this.avatarUrl,
    lengthUnit: String = this.lengthUnit,
    weightUnit: String = this.weightUnit): Option[User] = {
    val map = List(
      diff(_.password, password),
      diff(_.firstName, firstName),
      diff(_.lastName, lastName),
      diff(_.avatarUrl, avatarUrl),
      diff(_.lengthUnit, lengthUnit),
      diff(_.weightUnit, weightUnit)
    ).flatten.toMap
    TABLE.update(id, map)
  }
}
object User extends AutoIDTable[User]("USER") {
  val email = Column[String]("EMAIL", (_.email), (_.getString.get), attrString)
  val password = Column[Option[String]]("PASSWORD", (_.password), (_.getString), attrString)
  val firstName = Column[String]("FIRST_NAME", (_.firstName), (_.getString.get), attrString)
  val lastName = Column[String]("LAST_NAME", (_.lastName), (_.getString.get), attrString)
  val avatarUrl = Column[Option[String]]("AVATAR_URL", (_.avatarUrl), (_.getString), attrString)
  val lengthUnit = Column[String]("LENGTH_UNIT", (_.lengthUnit), (_.getString.get), attrString)
  val weightUnit = Column[String]("WEIGHT_UNIT", (_.weightUnit), (_.getString.get), attrString)
  // All columns
  val columns = List(password, firstName, lastName, avatarUrl, lengthUnit, weightUnit)
  /**
   * Add new user
   */
  def addNew(theEmail: String,
    unhashedPassword: Option[String],
    theFirstName: String,
    theLastName: String,
    theAvatarUrl: Option[String] = None,
    theLengthUnit: String = "cm",
    theWeightUnit: String = "kg"): User = {
    addNew(
      email(theEmail),
      password(unhashedPassword.map(hash)),
      firstName(theFirstName),
      lastName(theLastName),
      avatarUrl(theAvatarUrl),
      lengthUnit(theLengthUnit),
      weightUnit(theWeightUnit)
    )
  }
  // Password hashing
  val hashingWay = "SHA-1"
  def hash(v: String): String = play.api.libs.Codecs.sha1(v)
}