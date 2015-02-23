import play.api.libs.json._

import com.amazonaws.services.dynamodbv2.document.{ DynamoDB, Item, PrimaryKey }
import com.amazonaws.services.dynamodbv2.document.spec.{ GetItemSpec, ScanSpec }

package object models {
  lazy val db = new DynamoDB(service.AWS.DynamoDB.client)

  def getTable(name: String) = db.getTable(f"TritonNote-${name}")

  def newItem(id: String, content: JsValue) = new Item().withPrimaryKey("ID", id).withJSON("CONTENT", content.toString)

  def getter(id: String) = new GetItemSpec().withPrimaryKey("ID", id)
  
  def contents = new ScanSpec().withAttributesToGet("CONTENT")

  def scanLast(maxSize: Int, last: Option[String]) =
    new ScanSpec().withMaxPageSize(maxSize).withExclusiveStartKey(last.map(new PrimaryKey("ID", _)).orNull)

  def generateId = play.api.libs.Crypto.generateToken
}