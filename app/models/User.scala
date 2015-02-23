package models

import play.api.libs.json._

case class User(id: String, name: String, measureUnit: ValueUnit.Measures, connections: Set[User.SocialConnection]) {
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
      case class SocialService(name: String) extends Val {
        def find(socialId: String): Option[User] = {
          None
        }
      }
      val FACEBOOK = SocialService("facebook")
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
}
