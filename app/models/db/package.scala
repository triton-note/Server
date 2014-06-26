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
      def get[O <: TimestampedTable.ObjType[Long]](t: AutoIDTable[O]): Option[O] = getLong.flatMap(t.get)
      def get[O <: TimestampedTable.ObjType[String]](t: AnyIDTable[O]): Option[O] = getString.flatMap(t.get)
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
    case class Column[A](name: String, getProp: T => A, valueOf: AttributeValueWrapper => A, toAttr: A => AttributeValue) {
      def apply(a: A): (String, AttributeValue) = name -> toAttr(a)
      def build(implicit map: Map[String, AttributeValue]): A = valueOf(new AttributeValueWrapper(map.getOrElse(name, new AttributeValue)))
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
    def find(attributes: Map[String, AttributeValue]): Set[T] = {
      val q = new QueryRequest(tableName).withKeyConditions(attributes map {
        case (n, v) => n -> new Condition().withComparisonOperator(ComparisonOperator.EQ).withAttributeValueList(v)
      })
      for {
        result <- Option(client query q).toSet
        if { Logger.debug(f"Found ${tableName} ${result}"); true }
        item <- Option(result.getItems).toSet.flatten
        o <- fromMap(item.toMap)
      } yield o
    }
    def scan[A](conditions: Map[String, Condition], names: String*)(convert: Map[String, AttributeValue] => Option[A]): Set[A] = {
      val request = new ScanRequest().withTableName(tableName).withAttributesToGet(names).withScanFilter(conditions)
      try {
        for {
          result <- Option(client scan request).toSet
          if { Logger.debug(f"Result of scaning ${tableName}: ${result}"); true }
          item <- result.getItems
          o <- convert(item.toMap)
        } yield o
      } catch {
        case ex: Exception =>
          Logger.error(f"Failed to scan items: ${ex.getMessage}", ex)
          Set()
      }
    }
    def scan(conditions: Map[String, Condition]): Set[T] = {
      scan(conditions, allColumns.map(_.name): _*)(fromMap(_))
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
    def delete(i: K): Boolean = {
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
    def get(i: K): Option[T] = {
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
    def update(i: K, attributes: Map[String, AttributeValue])(implicit expected: Map[String, ExpectedAttributeValue] = Map()): Option[T] = {
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
    def putNew(attributes: Map[String, AttributeValue]): Option[T] = {
      val map = (attributes - createdAt.name - lastModifiedAt.name + createdAt(currentTimestamp)).filter(!_._2.isEmpty)
      Logger debug f"Putting ${tableName} by ${map}"
      try {
        for {
          result <- Option(client.putItem(tableName, map))
          if { Logger.debug(f"Result of putting ${tableName}: ${result}"); true }
          o <- fromMap(map)
        } yield o
      } catch {
        case ex: Exception =>
          Logger error (f"Failed to put ${tableName}", ex)
          None
      }
    }
  }
  abstract class AnyIDTable[T <: TimestampedTable.ObjType[String]](val tableName: String) extends TimestampedTable[String, T] {
    /**
     * Column 'ID'
     */
    val id = Column[String]("ID", (_.id), (_.getString.get), attrString)
    /**
     * Add item.
     *
     * @return New item which has proper id.
     */
    def addNew(key: String, attributes: (String, AttributeValue)*): Option[T] = {
      putNew(attributes.toMap - id.name + id(key))
    }
  }
  abstract class AutoIDTable[T <: TimestampedTable.ObjType[Long]](val tableName: String) extends TimestampedTable[Long, T] {
    /**
     * Column 'ID'
     */
    val id = Column[Long]("ID", (_.id), (_.getLong.get), attrLong)
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
      putNew(attributes.toMap - id.name + id(generateID))
    }
  }
}