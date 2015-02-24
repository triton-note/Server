package models

import scala.collection.JavaConversions._

import play.api.libs.json._

case class User(id: String, username: String, measureUnit: ValueUnit.Measures, connections: Set[User.SocialConnection]) {
  def save: Option[User] = User.save(this)
  /**
   * Connect to specified social service
   */
  def connect(service: User.SocialConnection.Service.Value, id: String): Option[User] = {
    val (found, left) = connections.partition(_.service == service)
    found.headOption match {
      case Some(found) if !found.connected =>
        val add = found.copy(connected = true)
        copy(connections = left + add).save
      case None =>
        val add = User.SocialConnection(service, id, true)
        copy(connections = left + add).save
      case _ => Option(this)
    }
  }
}
object User {
  case class SocialConnection(service: SocialConnection.Service.Value, accountId: String, connected: Boolean)
  object SocialConnection {
    object Service extends Enumeration {
      val FACEBOOK = Value("facebook")
      implicit val json = Format[Service.Value](
        (__).read[String].map(Service.withName),
        Writes { Json toJson _.toString })
    }
    implicit val json = Json.format[SocialConnection]
  }
  implicit val json = Json.format[User]

  /**
   *  Connect to DynamoDB Table
   */
  lazy val DB = new TableDelegate("USER")

  def create(name: String, measureUnit: ValueUnit.Measures, connections: User.SocialConnection*) = {
    User(generateId, name, measureUnit, connections.toSet).save.get
  }
  def save(user: User): Option[User] = DB save user
  def get(id: String): Option[User] = DB get id
  def findBy(social: SocialConnection.Service.Value)(socialId: String): Option[User] = {
    DB.stream()(_.withFilterExpression(f"${DB.json("connections", "accountId")} = :acountId").withValueMap(Map(":accountId" -> socialId))).headOption
  }
}
