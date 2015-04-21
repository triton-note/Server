package models

import scala.collection.JavaConversions._
import scala.concurrent.Future

import play.api.libs.json._

import com.amazonaws.services.dynamodbv2.document.utils.ValueMap

case class User(id: String, cognitoId: String, measureUnit: ValueUnit.Measures) {
  def save: Option[User] = User.save(this)
}
object User {
  implicit val json = Json.format[User]

  /**
   *  Connect to DynamoDB Table
   */
  lazy val DB = new TableDelegate("USER")

  def create(cognitoId: String,
    measureUnit: ValueUnit.Measures = ValueUnit.Measures(ValueUnit.Length.Measure.CM, ValueUnit.Weight.Measure.KG, ValueUnit.Temperature.Measure.Cels)) = {
    User(generateId, cognitoId, measureUnit).save.get
  }
  def get(id: String): Option[User] = DB get id
  def save(user: User): Option[User] = DB.save(user)(_.withString("COGNITO_ID", user.cognitoId))

  def findBy(cognitoId: String): Option[User] = {
    DB.scan(_
      .withFilterExpression(f"#n1 = :v1")
      .withNameMap(Map(
        "#n1" -> "COGNITO_ID"
      ))
      .withValueMap(new ValueMap()
        .withString(":v1", cognitoId)
      )
    ).headOption
  }
}
