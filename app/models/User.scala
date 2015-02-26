package models

import scala.collection.JavaConversions._

import play.api.libs.json._

import com.amazonaws.services.dynamodbv2.document.utils.ValueMap

case class User(id: String, username: String, measureUnit: ValueUnit.Measures, connections: List[User.SocialConnection]) {
  def save: Option[User] = User.save(this)
  /**
   * Connect to specified social service
   */
  def connect(service: User.SocialConnection.Service.Value, id: String): Option[User] = {
    connections.partition(_.service == service) match {
      case (Nil, left) =>
        val add = User.SocialConnection(service, id)
        copy(connections = add :: left).save
      case _ => Option(this)
    }
  }
}
object User {
  case class SocialConnection(service: SocialConnection.Service.Value, accountId: String)
  object SocialConnection {
    object Service extends Enumeration {
      val FACEBOOK = Value("facebook")
      implicit val json = Format[Value](
        Reads.verifying[String](values.map(_.toString).contains).map(withName),
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
    User(generateId, name, measureUnit, connections.toList).save.get
  }
  def save(user: User): Option[User] = DB save user
  def get(id: String): Option[User] = DB get id
  def findBy(social: SocialConnection.Service.type => SocialConnection.Service.Value)(socialId: String): Option[User] = {
    DB.scan(_
      .withFilterExpression(f"contains(CONTENT.#n1, :v1)")
      .withNameMap(Map(
        "#n1" -> "connections"
      ))
      .withValueMap(Map(
        ":v1" -> new ValueMap()
          .withString("service", social(SocialConnection.Service).toString)
          .withString("accountId", socialId)
      ))
    ).headOption
  }
}
