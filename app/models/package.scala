import scala.collection.JavaConversions._

import play.api.Logger
import play.api.libs.json._

import org.fathens.play.util.Exception.allCatch

import com.amazonaws.services.dynamodbv2.document.{ Item, PrimaryKey, Table }
import com.amazonaws.services.dynamodbv2.document.spec.{ GetItemSpec, QuerySpec, ScanSpec }

import service.AWS.DynamoDB

package object models {
  def generateId = play.api.libs.Crypto.generateToken

  class TableDelegate[T <: { val id: String }](name: String)(implicit $writer: Format[T]) {
    lazy val TABLE = DynamoDB.getTable(f"TritonNote.${name}")
    val ID = "ID"
    val CONTENT = "CONTENT"

    private def withTable[A](p: Table => A): Option[A] = allCatch.opt(Option(p(TABLE))).flatten

    implicit def itemsToT(items: java.lang.Iterable[Item]): Stream[T] = for {
      item <- items.iterator().toStream
      json <- allCatch opt { Json parse item.getJSON(CONTENT) }
      t <- json.asOpt[T]
    } yield t

    def save(content: T)(implicit alpha: Item => Item): Option[T] = {
      val item = alpha(new Item().withPrimaryKey(ID, content.id).withJSON(CONTENT, (Json toJson content).toString))
      val result = withTable(_ putItem item)
      Logger trace f"Save: ${TABLE.getTableName}(${content.id}) => ${result}"
      result.map(_ => content)
    }
    def get(id: String): Option[T] = {
      val getter = new GetItemSpec().withPrimaryKey(ID, id).withAttributesToGet(CONTENT)
      val result = withTable(_ getItem getter)
      Logger trace f"Get: ${TABLE.getTableName}(${id}) => ${result}"
      result.map(_ getJSON CONTENT).flatMap { text =>
        allCatch.opt(Json parse text).map(_.as[T])
      }
    }
    def delete(id: String): Boolean = {
      val key = new PrimaryKey(ID, id)
      val result = withTable(_ deleteItem key)
      Logger trace f"Deleted: ${TABLE.getTableName}(${id}) => ${result}"
      result.isDefined
    }
    def scan(alpha: ScanSpec => ScanSpec = identity): Stream[T] = {
      TABLE scan alpha(new ScanSpec)
    }
    def query(indexName: String)(alpha: QuerySpec => QuerySpec): Stream[T] = {
      Logger trace f"Querying: ${TABLE.getTableName}:${indexName}"
      TABLE.getIndex(indexName) query alpha(new QuerySpec)
    }
  }
}
