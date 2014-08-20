package models.db

import scala.collection.JavaConversions._

import com.amazonaws.services.dynamodbv2.model._

case class User(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[User] {
  val TABLE = User

  lazy val email: String = build(_.email)
  lazy val name: String = build(_.name)
  lazy val avatarUrl: Option[String] = build(_.avatarUrl)
  lazy val lengthUnit: String = build(_.lengthUnit)
  lazy val weightUnit: String = build(_.weightUnit)
  /**
   * Change properties (like a copy) and update Database
   */
  def update(
    email: String = this.email,
    name: String = this.name,
    avatarUrl: Option[String] = this.avatarUrl,
    lengthUnit: String = this.lengthUnit,
    weightUnit: String = this.weightUnit): Option[User] = {
    val map = List(
      diff(_.email, email),
      diff(_.name, name),
      diff(_.avatarUrl, avatarUrl),
      diff(_.lengthUnit, lengthUnit),
      diff(_.weightUnit, weightUnit)
    ).flatten.toMap
    TABLE.update(id, map)
  }
}
object User extends AutoIDTable[User]("USER") {
  val email = Column[String]("EMAIL", (_.email), (_.getString.get), attrString)
  val name = Column[String]("NAME", (_.name), (_.getString.get), attrString)
  val avatarUrl = Column[Option[String]]("AVATAR_URL", (_.avatarUrl), (_.getString), attrString)
  val lengthUnit = Column[String]("LENGTH_UNIT", (_.lengthUnit), (_.getString.get), attrString)
  val weightUnit = Column[String]("WEIGHT_UNIT", (_.weightUnit), (_.getString.get), attrString)
  // All columns
  val columns = List(name, avatarUrl, lengthUnit, weightUnit)
  /**
   * Add new user
   */
  def addNew(theEmail: String,
    theName: String,
    theAvatarUrl: Option[String] = None,
    theLengthUnit: String = "cm",
    theWeightUnit: String = "kg"): User = {
    addNew(
      email(theEmail),
      name(theName),
      avatarUrl(theAvatarUrl),
      lengthUnit(theLengthUnit),
      weightUnit(theWeightUnit)
    )
  }
}

case class SocialConnection(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[SocialConnection] {
  val TABLE = SocialConnection

  lazy val accountId: String = build(_.accountId)
  lazy val service: SocialConnection.Service.Value = TABLE.service build MAP
  lazy val user: Option[User] = build(_.user)
}
object SocialConnection extends AutoIDTable[SocialConnection]("SOCIAL_CONNECTION") {
  val accountId = Column[String]("ACCOUNT_ID", (_.accountId), (_.getString.get), attrString)
  val service = Column[Service.Value]("SERVICE", (_.service), (Service withName _.getString.get), attrEnum(Service))
  val user = Column[Option[User]]("USER", (_.user), (_.get(User)), attrObjID)
  // All columns
  val columns = List(accountId)

  def addNew(theAccountId: String, theService: Service.Value, theUser: User): SocialConnection = {
    addNew(
      accountId(theAccountId),
      service(theService),
      user(Option(theUser))
    )
  }

  def findBy(theAccountId: String, theService: Service.Value): Option[SocialConnection] = {
    find(_.withIndexName("ACCOUNT_ID-SERVICE-index").withKeyConditions(Map(
      accountId compare theAccountId,
      service compare theService
    ))).headOption
  }
  def findBy(theUser: User): List[SocialConnection] = {
    find(_.withIndexName("USER-index").withKeyConditions(Map(
      user compare Option(theUser)
    ))).toList
  }

  object Service extends Enumeration {
    val FACEBOOK = Value("facebook")
    val GOOGLE = Value("google")
  }
}
