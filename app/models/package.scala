import play.api.libs.json._

import org.fathens.play.util.Exception.allCatch

import com.amazonaws.services.dynamodbv2.document.{ DynamoDB, Item, PrimaryKey, Table }
import com.amazonaws.services.dynamodbv2.document.spec.{ GetItemSpec, ScanSpec }

package object models {
  lazy val db = new DynamoDB(service.AWS.DynamoDB.client)

  def generateId = play.api.libs.Crypto.generateToken

  class TableDelegate[T <: { val id: String }](name: String)(implicit $writer: Format[T]) {
    val TABLE = db.getTable(f"TritonNote-${name}")

    private def optCatch[A](p: Table => A): Option[A] = allCatch.opt(Option(p(TABLE))).flatten

    def save(content: T): Option[T] = {
      val item = new Item().withPrimaryKey("ID", content.id).withJSON("CONTENT", (Json toJson content).toString)
      val result = optCatch(_ putItem item)
      result.map(_ => content)
    }
    def get(id: String): Option[T] = {
      val getter = new GetItemSpec().withPrimaryKey("ID", id).withAttributesToGet("CONTENT")
      val result = optCatch(_ getItem getter)
      result.map(_ getJSON "CONTENT").flatMap { text =>
        allCatch.opt(Json parse text).map(_.as[T])
      }
    }
    def delete(id: String): Boolean = {
      val key = new PrimaryKey("ID", id)
      val result = optCatch(_ deleteItem key)
      result.isDefined
    }
  }

  def contents = new ScanSpec().withAttributesToGet("CONTENT")

  def scanLast(maxSize: Int, last: Option[String]) =
    new ScanSpec().withMaxPageSize(maxSize).withExclusiveStartKey(last.map(new PrimaryKey("ID", _)).orNull)
}
