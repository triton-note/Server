package models.db

import scala.collection.JavaConversions._

import com.amazonaws.services.dynamodbv2.model._

import models.ValueUnit

case class User(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[User] {
  val TABLE = User

  lazy val name: String = build(_.name)
  lazy val measureUnit: ValueUnit.Measures = build(_.measureUnit)
  /**
   * Change properties (like a copy) and update Database
   */
  def update(
    name: String = this.name,
    measureUnit: ValueUnit.Measures = this.measureUnit): Option[User] = {
    val map = List(
      diff(_.name, name),
      diff(_.measureUnit, measureUnit)
    ).flatten.toMap
    TABLE.update(id, map)
  }
}
object User extends AutoIDTable[User]("USER") {
  val name = Column[String]("NAME", (_.name), (_.getString.get), attrString)
  val measureUnit = Column[ValueUnit.Measures]("MEASURE_UNIT", (_.measureUnit), (_.getJson.get.as[ValueUnit.Measures]), (_.asJson))
  // All columns
  val columns = List(name, measureUnit)
  /**
   * Add new user
   */
  def addNew(theName: String,
    theMeasureUnit: ValueUnit.Measures = ValueUnit.Measures(
      ValueUnit.Length.Measure.CM,
      ValueUnit.Weight.Measure.KG,
      ValueUnit.Temperature.Measure.Cels
    )): User = {
    addNew(
      name(theName),
      measureUnit(theMeasureUnit)
    )
  }
}

case class SocialConnection(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[SocialConnection] {
  val TABLE = SocialConnection

  lazy val accountId: String = build(_.accountId)
  lazy val service: SocialConnection.Service.Value = TABLE.service build MAP
  lazy val connected: Boolean = build(_.connected)
  lazy val user: Option[User] = build(_.user)
  /**
   * Change properties (like a copy) and update Database
   */
  def update(
    accountId: String = this.accountId,
    connected: Boolean = this.connected): Option[SocialConnection] = {
    val map = List(
      diff(_.accountId, accountId),
      diff(_.connected, connected)
    ).flatten.toMap
    if (map.isEmpty) Some(this) else TABLE.update(id, map)
  }
  def connect: Option[SocialConnection] = update(connected = true)
  def disconnect: Option[SocialConnection] = update(connected = false)
}
object SocialConnection extends AutoIDTable[SocialConnection]("SOCIAL_CONNECTION") {
  val accountId = Column[String]("ACCOUNT_ID", (_.accountId), (_.getString.get), attrString)
  val service = Column[Service.Value]("SERVICE", (_.service), (Service withName _.getString.get), attrEnum(Service))
  val connected = Column[Boolean]("CONNECTED", (_.connected), (_.getBoolean), attrBoolean)
  val user = Column[Option[User]]("USER", (_.user), (_.get(User)), attrObjID)
  // All columns
  val columns = List(accountId)

  def addNew(theAccountId: String, theService: Service.Value, theUser: User): SocialConnection = {
    addNew(
      accountId(theAccountId),
      service(theService),
      connected(true),
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
