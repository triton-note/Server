package models

import java.util.Date

import scala.annotation.tailrec
import scala.collection.JavaConversions.{ asScalaBuffer, mapAsJavaMap, mapAsScalaMap, setAsJavaSet }

import scalaz.Scalaz._

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
    implicit class AttributeValueOpt(av: AttributeValue) {
      def getDouble: Double = av.getN.toDouble
      def getLong: Long = av.getN.toLong
      def getDate: Date = av.getS.some.map(dateFormat.parse).orNull
      def get[O <: TimestampedTable.ObjType[Long]](t: AutoIDTable[O]): Option[O] = t.get(av.getLong)
      def get[O <: TimestampedTable.ObjType[String]](t: AnyIDTable[O]): Option[O] = t.get(av.getS)
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
    implicit def attrObjLongID(o: Option[TimestampedTable.ObjType[Long]]) = new AttributeValue().withN(o.map(_.id).map(numberFormat).orNull)
    implicit def attrObjStringID(o: Option[TimestampedTable.ObjType[String]]) = new AttributeValue().withN(o.map(_.id).orNull)
    /**
     * Representation of column.
     */
    case class Column[A](name: String, getProp: T => A, valueOf: AttributeValue => A, toAttr: A => AttributeValue) {
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
     * Table name
     */
    val tableName: String
    /**
     * Find item by attributes given.
     */
    def find(attributes: (String, AttributeValue)*): Set[T] = find(attributes.toMap)
    def find(attributes: Map[String, AttributeValue]): Set[T] = {
      val q = new QueryRequest(tableName).withKeyConditions(attributes map {
        case (n, v) => n -> new Condition().withComparisonOperator(ComparisonOperator.EQ).withAttributeValueList(v)
      })
      for {
        result <- Option(client query q).toSet
        item <- result.getItems
        o <- fromMap(item.toMap)
      } yield o
    }
  }
  object TimestampedTable {
    type ObjType[K] = {
      val id: K
      val createdAt: Date
      val lastModifiedAt: Option[Date]
    }
  }
  trait TimestampedTable[K, T <: TimestampedTable.ObjType[K]] extends TableRoot[T] {
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
    lazy val allColumns: List[Column[_]] = superColumns ::: columns
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
    def update(i: K, attributes: Map[String, AttributeValue])(implicit expected: Map[String, ExpectedAttributeValue] = Map()): Option[T] = {
      val key = Map(id(i))
      try {
        val request = {
          val u = for {
            (n, v) <- attributes.toMap - id.name - createdAt.name + lastModifiedAt(Option(currentTimestamp))
          } yield n -> new AttributeValueUpdate().withAction(AttributeAction.ADD).withValue(v)
          new UpdateItemRequest(tableName, key, u, "ALL_NEW").withExpected(expected)
        }
        for {
          result <- Option(client updateItem request)
          item <- Option(result.getAttributes)
          if item.nonEmpty
          o <- fromMap(item.toMap)
        } yield o
      } catch {
        case ex: Exception => None
      }
    }
  }
  abstract class AnyIDTable[T <: TimestampedTable.ObjType[String]](val tableName: String) extends TimestampedTable[String, T] {
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
  abstract class AutoIDTable[T <: TimestampedTable.ObjType[Long]](val tableName: String) extends TimestampedTable[Long, T] {
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