package models

import scala.collection.JavaConversions._
import scala.concurrent.Future

import play.api.libs.json._

import com.amazonaws.services.dynamodbv2.document.utils.ValueMap

case class User(id: String, username: String, measureUnit: ValueUnit.Measures, connections: Set[User.SocialConnection]) {
  def save: Option[User] = User.save(this)

  /**
   * Connect to specified social service
   */
  def connect(service: User.SocialConnection.Service.Value, id: String): Option[User] = {
    connections.toList.partition(_.service == service) match {
      case (found :: Nil, left) if !found.connected =>
        val add = found.copy(connected = true)
        copy(connections = left.toSet + add).save
      case (Nil, left) =>
        val add = User.SocialConnection(service, id, true)
        copy(connections = left.toSet + add).save
      case _ => Option(this)
    }
  }
  /**
   * Disconnect to specified social service.
   * This just make connected flag to false.
   */
  def disconnect(service: User.SocialConnection.Service.Value): Option[User] = {
    connections.toList.partition(_.service == service) match {
      case (social :: Nil, left) if social.connected =>
        val add = social.copy(connected = false)
        copy(connections = left.toSet + add).save
      case _ => Option(this)
    }
  }
}
object User {
  case class SocialConnection(service: SocialConnection.Service.Value, accountId: String, connected: Boolean)
  object SocialConnection {
    object Service extends Enumeration {
      val FACEBOOK = Value("facebook")(service.Facebook User _)
      
      def apply(name: String) = withName(name).asInstanceOf[Value]
      trait Value { self: super.Val =>
        def connect(accessKey: String): Future[Option[User]]
      }
      def Value(name: String)(connector: String => Future[Option[User]]) = new Val(nextId, name) with Value {
        def connect(accessKey: String) = connector(accessKey)
      }
      implicit val json = Format[Value](
        Reads.verifying[String](values.map(_.toString).contains).map(apply),
        Writes { Json toJson _.toString })
    }
    def of(social: SocialConnection.Service.type => SocialConnection.Service.Value, socialId: String, connected: Boolean): SocialConnection = {
      apply(social(Service), socialId, connected)
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
  def findBy(social: SocialConnection.Service.type => SocialConnection.Service.Value, socialId: String, connected: Boolean): Option[User] = {
    DB.scan(_
      .withFilterExpression(f"contains(CONTENT.#n1, :v1)")
      .withNameMap(Map(
        "#n1" -> "connections"
      ))
      .withValueMap(Map(
        ":v1" -> new ValueMap()
          .withString("service", social(SocialConnection.Service).toString)
          .withString("accountId", socialId)
          .withBoolean("connected", connected)
      ))
    ).headOption
  }
}
