package models

import scala.collection.JavaConversions._
import scala.concurrent.Future

import play.api.libs.json._

import com.amazonaws.services.dynamodbv2.document.utils.ValueMap

case class User(id: String, measureUnit: ValueUnit.Measures) {
  def save: Option[User] = User.save(this)
}
object User {
  implicit val json = Json.format[User]

  /**
   *  Connect to DynamoDB Table
   */
  lazy val DB = new TableDelegate("USER")

  def create(id: String, 
      measureUnit: ValueUnit.Measures = ValueUnit.Measures(ValueUnit.Length.Measure.CM, ValueUnit.Weight.Measure.KG, ValueUnit.Temperature.Measure.Cels)) = {
    User(id, measureUnit).save.get
  }
  def save(user: User): Option[User] = DB save user
  def get(id: String): Option[User] = DB get id
}
