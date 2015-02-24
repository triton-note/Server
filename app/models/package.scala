import scala.collection.JavaConversions._

import play.api.Logger
import play.api.libs.json._

import org.fathens.play.util.Exception.allCatch

import com.amazonaws.services.dynamodbv2.document.{ DynamoDB, Item, PrimaryKey, Table }
import com.amazonaws.services.dynamodbv2.document.spec.{ GetItemSpec, ScanSpec }

package object models {
  lazy val db = new DynamoDB(service.AWS.DynamoDB.client)

  def generateId = play.api.libs.Crypto.generateToken

  class TableDelegate[T <: { val id: String }](name: String)(implicit $writer: Format[T]) {
    lazy val TABLE = db.getTable(f"TritonNote.${name}")
    val ID = "ID"
    val CONTENT = "CONTENT"

    private def optCatch[A](p: Table => A): Option[A] = allCatch.opt(Option(p(TABLE))).flatten

    def save(content: T): Option[T] = {
      val item = new Item().withPrimaryKey(ID, content.id).withJSON(CONTENT, (Json toJson content).toString)
      val result = optCatch(_ putItem item)
      Logger trace f"Save: ${TABLE.getTableName}(${content.id}) => ${result}"
      result.map(_ => content)
    }
    def get(id: String): Option[T] = {
      val getter = new GetItemSpec().withPrimaryKey(ID, id).withAttributesToGet(CONTENT)
      val result = optCatch(_ getItem getter)
      Logger trace f"Get: ${TABLE.getTableName}(${id}) => ${result}"
      result.map(_ getJSON CONTENT).flatMap { text =>
        allCatch.opt(Json parse text).map(_.as[T])
      }
    }
    def delete(id: String): Boolean = {
      val key = new PrimaryKey(ID, id)
      val result = optCatch(_ deleteItem key)
      Logger trace f"Deleted: ${TABLE.getTableName}(${id}) => ${result}"
      result.isDefined
    }
    def paging(limit: Int, last: Option[String])(alpha: ScanSpec => ScanSpec): Stream[T] = {
      val start = last.map(key => new PrimaryKey(ID, key)).orNull
      Logger trace f"Paging: ${TABLE.getTableName}: pageSize=${limit}: from=${start}"
      val spec = alpha(new ScanSpec).withMaxResultSize(limit).withExclusiveStartKey(start)
      for {
        item <- TABLE.scan(spec).toStream
        json <- allCatch opt { Json parse item.getJSON(CONTENT) }
        t <- json.asOpt[T]
      } yield t
    }
    def stream(pageSize: Int = 100)(alpha: ScanSpec => ScanSpec = identity): Stream[T] = {
      def mkStream(p: Option[String] => Stream[T]): Stream[T] = {
        def db(a: Stream[T]): Stream[Stream[T]] = a #:: {
          a.lastOption match {
            case Some(t) => db(p(Some(t.id)))
            case _       => Stream.empty
          }
        }
        db(p(None)).flatten
      }
      mkStream { paging(pageSize, _)(alpha) }
    }
    /**
     * Create path on json value
     */
    def json(path: String*) = (CONTENT :: path.toList).mkString(".")
  }
}
