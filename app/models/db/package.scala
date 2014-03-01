package models

import java.util.Date
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import com.amazonaws.services.dynamodbv2.model._
import scala.collection.JavaConversions._
import service.AWS.DynamoDB._
import scala.annotation.tailrec

package object db {
  def currentTimestamp = new Date
  def numberFormat(n: Double) = f"$n%.10f"
  def numberFormat(n: Long) = f"$n%d"
  val dateFormat = new {
    val du = new com.amazonaws.util.DateUtils
    def format(date: Date) = du.formatIso8601Date(date)
    def parse(s: String) = du.parseIso8601Date(s)
  }
}
package db {
  trait TableRoot[T] {
    /**
     * Extension for AttribueValue
     */
    implicit class AttributeValueOpt(av: AttributeValue) {
      def getDouble: Double = av.getN.toDouble
      def getLong: Long = av.getN.toLong
      def getDate: Date = av.getS.some.map(dateFormat.parse).orNull
      def get[O](t: AutoIDTable[O]): Option[O] = t.get(av.getLong)
      def get[O](t: AnyIDTable[O]): Option[O] = t.get(av.getS)
    }
    // for String
    implicit def attrString(s: String) = new AttributeValue().withS(s)
    implicit def attrString(s: Option[String]) = new AttributeValue().withS(s.orNull)
    implicit def attrString(ss: Set[String]) = new AttributeValue().withSS(ss)
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
    implicit def attrObjLongID(o: Option[TimestampledTable.ObjType[Long]]) = new AttributeValue().withN(o.map(_.id).map(numberFormat).orNull)
    implicit def attrObjStringID(o: Option[TimestampledTable.ObjType[String]]) = new AttributeValue().withN(o.map(_.id).orNull)
    /**
     * Representation of column.
     */
    case class Column[A](name: String, getProp: T => A, valueOf: AttributeValue => A, toAttr: A => AttributeValue) {
      def apply(obj: T): (String, AttributeValue) = apply(getProp(obj))
      def apply(a: A): (String, AttributeValue) = name -> toAttr(a)
      def build(implicit map: Map[String, AttributeValue]): A = valueOf(map(name))
    }
    /**
     * All columns
     */
    val allColumns: List[Column[_]]
    /**
     * Mapping to Object from AttributeValues
     * return None if failure.
     */
    def fromMap(implicit map: Map[String, AttributeValue]): Option[T]
    /**
     * Mapping to AttributeValues from Object
     */
    def toMap(obj: T): Map[String, AttributeValue] = allColumns.map(_(obj)).toMap
    /**
     * Table name
     */
    val tableName: String
  }
  object TimestampledTable {
    type ObjType[K] = {
      val id: K
      val createdAt: Date
      val lastModifiedAt: Option[Date]
    }
  }
  abstract class TimestampedTable[K, T <: TimestampledTable.ObjType[K]](val tableName: String) extends TableRoot[T] {
    val id: Column[K]
    /**
     * Column 'CREATED_AT'
     */
    val createdAt = Column[Date]("CREATED_AT", (_.createdAt), (_.getDate), attrDate)
    /**
     * Column 'LAST_MODIFIED_AT'
     */
    val lastModifiedAt = Column[Option[Date]]("LAST_MODIFIED_AT", (_.lastModifiedAt), (_.getDate.some), attrDate)
    /**
     * Columns at this class
     */
    val superColumns = List(id, createdAt, lastModifiedAt)
    /**
     * Other columns of which is defined in subclass.
     */
    val columns: List[Column[_]]
    /**
     * All columns
     */
    val allColumns: List[Column[_]] = superColumns ::: columns
    /**
     * Delete item.
     *
     * @return true if successfully done
     */
    def delete(i: K): Boolean = {
      val key = Map(id(i))
      try {
        client.deleteItem(tableName, key)
        true
      } catch {
        case ex: Exception => false
      }
    }
    /**
     * Get(Find) item by id
     */
    def get(i: K): Option[T] = {
      val key = Map(id(i))
      try {
        for {
          result <- Option(client.getItem(tableName, key, true))
          item <- Option(result.getItem)
          if item.nonEmpty
          o <- fromMap(item.toMap)
        } yield o
      } catch {
        case ex: Exception => None
      }
    }
    /**
     * Update given attributes of item by specified id.
     *
     * @return updated item
     */
    def update(obj: T): Option[T] = update(obj.id, columns.map(_(obj)).toMap)
    def update(i: K, attributes: (String, AttributeValue)*): Option[T] = update(i, attributes.toMap)
    def update(i: K, attributes: Map[String, AttributeValue]): Option[T] = {
      val key = Map(id(i))
      try {
        val u = {
          attributes.toMap - id.name - createdAt.name +
            lastModifiedAt(Option(currentTimestamp))
        }.map {
          case (n, v) => n -> new AttributeValueUpdate().withAction(AttributeAction.ADD).withValue(v)
        }
        for {
          result <- Option(client.updateItem(tableName, key, u, "ALL_NEW"))
          item <- Option(result.getAttributes)
          if item.nonEmpty
          o <- fromMap(item.toMap)
        } yield o
      } catch {
        case ex: Exception => None
      }
    }
    /**
     * Find item by attributes given.
     */
    def find(attributes: (String, AttributeValue)*): Set[T] = find(attributes.toMap)
    def find(attributes: Map[String, AttributeValue]): Set[T] = {
      val q = new QueryRequest(tableName).withKeyConditions(attributes map {
        case (n, v) => n -> new Condition().withComparisonOperator(ComparisonOperator.EQ).withAttributeValueList(v)
      })
      for {
        result <- Option(client.query(q)).toSet
        item <- result.getItems
        o <- fromMap(item.toMap)
      } yield o
    }
  }
  abstract class AnyIDTable[T <: TimestampledTable.ObjType[String]](tableName: String) extends TimestampedTable[String, T](tableName) {
    /**
     * Column 'ID'
     */
    val id = Column[String]("ID", (_.id), (_.getS), attrString)
    /**
     * Add item.
     *
     * @return New item which has proper id.
     */
    def addNew(key: String, attributes: (String, AttributeValue)*): Option[T] = {
      val map = (attributes.toMap - id.name - createdAt.name - lastModifiedAt.name) +
        id(key) + createdAt(currentTimestamp)
      try {
        for {
          result <- Option(client.putItem(tableName, map))
          o <- fromMap(map)
        } yield o
      } catch {
        case ex: Exception => None
      }
    }
  }
  abstract class AutoIDTable[T <: TimestampledTable.ObjType[Long]](tableName: String) extends TimestampedTable[Long, T](tableName) {
    /**
     * Column 'ID'
     */
    val id = Column[Long]("ID", (_.id), (_.getLong), attrLong)
    /**
     * Generating ID by random numbers.
     */
    def generateID: Long = {
      @tailrec
      def gen: Long = {
        val i = new Date().getTime + scala.util.Random.nextLong
        get(i) match {
          case None    => i
          case Some(_) => gen
        }
      }
      gen
    }
    /**
     * Add item.
     *
     * @return New item which has proper id.
     */
    def addNew(attributes: (String, AttributeValue)*): Option[T] = {
      val map = (attributes.toMap - id.name - createdAt.name - lastModifiedAt.name) +
        id(generateID) + createdAt(currentTimestamp)
      try {
        for {
          result <- Option(client.putItem(tableName, map))
          o <- fromMap(map)
        } yield o
      } catch {
        case ex: Exception => None
      }
    }
  }
}