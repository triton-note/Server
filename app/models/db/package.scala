package models

import java.util.Date

import scala.annotation.tailrec
import scala.collection.JavaConversions._

import scalaz.Scalaz._

import play.api.Logger

import org.fathens.play.util.Exception.allCatch

import com.amazonaws.services.dynamodbv2.model._

import service.AWS.DynamoDB.client

package object db {
  def currentTimestamp = new Date
  def numberFormat(n: Double) = f"$n%.10f"
  def numberFormat(n: Long) = f"$n%d"
  val dateFormat = new {
    def format(date: Date) = com.amazonaws.util.DateUtils.formatISO8601Date(date)
    def parse(s: String) = com.amazonaws.util.DateUtils.parseISO8601Date(s)
  }
}
package db {
  trait TableRoot[T] {
    /**
     * Extension for AttribueValue
     */
    implicit class AttributeValueExt(av: AttributeValue) {
      def isEmpty: Boolean = new AttributeValue == av
    }
    class AttributeValueWrapper(av: AttributeValue) {
      def getString: Option[String] = Option(av.getS)
      def getDouble: Option[Double] = Option(av.getN).flatMap(allCatch opt _.toDouble)
      def getLong: Option[Long] = Option(av.getN).flatMap(allCatch opt _.toLong)
      def getDate: Option[Date] = getString.flatMap(allCatch opt dateFormat.parse(_))
      def get[O <: TimestampedTable.ObjType[O]](t: TimestampedTable[O]): Option[O] = getString.flatMap(t.get)
    }
    // for String
    implicit def attrString(s: String) = new AttributeValue().withS(if (s != null && s.length > 0) s else null)
    implicit def attrString(s: Option[String]) = new AttributeValue().withS(s.flatMap(v => if (v != null && v.length > 0) Some(v) else None).orNull)
    implicit def attrString(ss: Set[String]) = new AttributeValue().withSS(ss.filter(s => s != null && s.length > 0))
    // for Double
    implicit def attrDouble(n: Double) = new AttributeValue().withN(numberFormat(n))
    implicit def attrDouble(n: Option[Double]) = new AttributeValue().withN(n.map(numberFormat).orNull)
    implicit def attrDouble(ns: Set[Double]) = new AttributeValue().withNS(ns map numberFormat)
    // for Long
    implicit def attrLong(n: Long) = new AttributeValue().withN(numberFormat(n))
    implicit def attrLong(n: Option[Long]) = new AttributeValue().withN(n.map(numberFormat).orNull)
    implicit def attrLong(ns: Set[Long]) = new AttributeValue().withNS(ns.map(numberFormat))
    // for Date
    implicit def attrDate(date: Date) = new AttributeValue().withS(dateFormat format date)
    implicit def attrDate(date: Option[Date]) = new AttributeValue().withS(date.map(dateFormat.format).orNull)
    // for ArrangedTableObj
    implicit def attrObjID(o: Option[TimestampedTable.ObjType[_]]) = new AttributeValue().withS(o.map(_.id).orNull)
    /**
     * Representation of column.
     */
    case class Column[A](name: String, getProp: T => A, valueOf: AttributeValueWrapper => A, toAttr: A => AttributeValue) {
      def apply(a: A): (String, AttributeValue) = name -> toAttr(a)
      def extract(obj: T): (String, AttributeValue) = name -> toAttr(getProp(obj))
      def build(implicit map: Map[String, AttributeValue]): A = valueOf(new AttributeValueWrapper(map.getOrElse(name, new AttributeValue)))
      def compare(a: A, co: ComparisonOperator = ComparisonOperator.EQ) = name -> new Condition().withComparisonOperator(co).withAttributeValueList(toAttr(a))
      def diff(a: A, b: A) = a != b option apply(b)
    }
    /**
     * All columns
     */
    val allColumns: List[Column[_]]
    /**
     * Mapping to Object from AttributeValues
     * @return None if failure.
     */
    def fromMap(implicit map: Map[String, AttributeValue]): Option[T]
    /**
     * Table name
     */
    val tableName: String
    /**
     * Query item by attributes given.
     */
    def find[A](q: QueryRequest => QueryRequest, convert: Map[String, AttributeValue] => Option[A] = (map: Map[String, AttributeValue]) => fromMap(map)): Set[A] = {
      try {
        for {
          result <- Option(client query q(new QueryRequest(tableName))).toSet
          if { Logger.debug(f"Found ${tableName} ${result}"); true }
          item <- Option(result.getItems).toSet.flatten
          o <- convert(item.toMap)
        } yield o
      } catch {
        case ex: Exception =>
          Logger.error(f"Failed to query to ${tableName}: ${ex.getMessage}", ex)
          Set()
      }
    }
    /**
     * Scan item by attributes given.
     */
    def scan[A](q: ScanRequest => ScanRequest, convert: Map[String, AttributeValue] => Option[A] = (map: Map[String, AttributeValue]) => fromMap(map)): Set[A] = {
      try {
        for {
          result <- Option(client scan q(new ScanRequest(tableName))).toSet
          if { Logger.debug(f"Found ${tableName} ${result}"); true }
          item <- Option(result.getItems).toSet.flatten
          o <- convert(item.toMap)
        } yield o
      } catch {
        case ex: Exception =>
          Logger.error(f"Failed to scan to ${tableName}: ${ex.getMessage}", ex)
          Set()
      }
    }
  }
  object TimestampedTable {
    trait ObjType[T <: TimestampedTable.ObjType[T]] { self: T =>
      val id: String
      val createdAt: Date
      val lastModifiedAt: Option[Date]
      val TABLE: TimestampedTable[T]
      /**
       * Reload from DB.
       * If there is no longer me, returns None.
       */
      def refresh: Option[T] = TABLE.get(id)
      /**
       * Delete me
       */
      def delete: Boolean = TABLE.delete(id)
      /**
       * Extract attributes from Object
       * @return Map of attributes
       */
      def toMap(needs: TABLE.Column[_]*): Map[String, AttributeValue] = {
        val list = needs match {
          case Nil => TABLE.allColumns
          case _   => needs
        }
        list.map(_ extract this).toMap
      }
    }
  }
  trait TimestampedTable[T <: TimestampedTable.ObjType[T]] extends TableRoot[T] {
    /**
     * Column 'ID'
     */
    val id = Column[String]("ID", (_.id), (_.getString.get), attrString)
    /**
     * Column 'CREATED_AT'
     */
    val createdAt = Column[Date]("CREATED_AT", (_.createdAt), (_.getDate.get), attrDate)
    /**
     * Column 'LAST_MODIFIED_AT'
     */
    val lastModifiedAt = Column[Option[Date]]("LAST_MODIFIED_AT", (_.lastModifiedAt), (_.getDate), attrDate)
    /**
     * Columns at this class
     */
    lazy val superColumns = List(id, createdAt, lastModifiedAt)
    /**
     * Other columns of which is defined in subclass.
     */
    val columns: List[Column[_]]
    /**
     * All columns
     */
    lazy val allColumns: List[Column[_]] = superColumns ::: columns
    /**
     * Delete item.
     *
     * @return true if successfully done
     */
    def delete(i: String): Boolean = {
      val key = Map(id(i))
      Logger debug f"Deleting ${tableName} by ${key}"
      try {
        client.deleteItem(tableName, key)
        true
      } catch {
        case ex: Exception =>
          Logger error (f"Failed to delete ${tableName}", ex)
          false
      }
    }
    /**
     * Get(Find) item by id
     */
    def get(i: String): Option[T] = {
      val key = Map(id(i))
      Logger debug f"Getting ${tableName} by ${key}"
      try {
        for {
          result <- Option(client.getItem(tableName, key, true))
          if { Logger.debug(f"Get ${tableName} ${result}"); true }
          item <- Option(result.getItem)
          o <- fromMap(item.toMap)
        } yield o
      } catch {
        case ex: Exception =>
          Logger error (f"Failed to get ${tableName}", ex)
          None
      }
    }
    /**
     * Update given attributes of item by specified id.
     *
     * @return updated item
     */
    def update(i: String, attributes: Map[String, AttributeValue])(implicit expected: Map[String, ExpectedAttributeValue] = Map()): Option[T] = {
      val key = Map(id(i))
      Logger debug f"Updating ${tableName} by ${key}"
      try {
        val request = {
          val u = for {
            (n, v) <- attributes.toMap - id.name - createdAt.name + lastModifiedAt(Option(currentTimestamp))
          } yield n -> new AttributeValueUpdate(v, AttributeAction.PUT)
          new UpdateItemRequest(tableName, key, u, "ALL_NEW").withExpected(expected)
        }
        for {
          result <- Option(client updateItem request)
          if { Logger.debug(f"Updated ${tableName} ${result}"); true }
          item <- Option(result.getAttributes)
          o <- fromMap(item.toMap)
        } yield o
      } catch {
        case ex: Exception =>
          Logger error (f"Failed to update ${tableName}", ex)
          None
      }
    }
    def putNew(attributes: Map[String, AttributeValue]): T = {
      val map = (attributes - createdAt.name - lastModifiedAt.name + createdAt(currentTimestamp)).filter(!_._2.isEmpty)
      Logger debug f"Putting ${tableName} by ${map}"
      val result = client.putItem(tableName, map)
      Logger debug f"Result of putting ${tableName}: ${result}"
      fromMap(map).get
    }
  }
  abstract class AnyIDTable[T <: TimestampedTable.ObjType[T]](val tableName: String) extends TimestampedTable[T] {
    /**
     * Add item.
     *
     * @return New item which has proper id.
     */
    def addNew(key: String, attributes: (String, AttributeValue)*): T = {
      putNew(attributes.toMap - id.name + id(key))
    }
  }
  abstract class AutoIDTable[T <: TimestampedTable.ObjType[T]](val tableName: String) extends TimestampedTable[T] {
    /**
     * Generating ID by random numbers.
     */
    @tailrec
    final def generateID: String = {
      val token = play.api.libs.Codecs.sha1(System.currentTimeMillis.toString)
      get(token) match {
        case None    => token
        case Some(_) => generateID
      }
    }
    /**
     * Add item.
     *
     * @return New item which has proper id.
     */
    def addNew(attributes: (String, AttributeValue)*): T = {
      putNew(attributes.toMap - id.name + id(generateID))
    }
  }
}