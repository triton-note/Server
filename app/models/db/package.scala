package models

import java.util.Date
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import com.amazonaws.services.dynamodbv2.model._
import scala.collection.JavaConversions._
import service.AWS.DynamoDB._

package object db {
  def currentTimestamp = new Date
  type TableObj = {
    val id: Long
    val createdAt: Date
    val lastModifiedAt: Option[Date]
  }
}
package db {
  abstract class Table[T <: TableObj](tableName: String) {
    def numberFormat(n: Double) = f"$n%.10f"
    val dateFormat = new {
      val du = new com.amazonaws.util.DateUtils
      def format(date: Date) = du.formatIso8601Date(date)
      def parse(s: String) = du.parseIso8601Date(s)
    }
    implicit class NullOption[A](a: A) {
      def option: Option[A] = Option(a)
    }
    implicit def attrStringOpt(s: Option[String]) = new AttributeValue().withS(s.orNull)
    implicit def attrString(s: String) = new AttributeValue().withS(s)
    implicit def attrDoubleOpt(n: Option[Double]) = new AttributeValue().withN(n.map(numberFormat).orNull)
    implicit def attrDouble(n: Double) = new AttributeValue().withN(numberFormat(n))
    implicit def attrLongOpt(n: Option[Long]) = new AttributeValue().withN(n.map(_.toString).orNull)
    implicit def attrLong(n: Long) = new AttributeValue().withN(n.toString)
    implicit def attrDateOpt(date: Option[Date]) = new AttributeValue().withS(date.map(dateFormat.format).orNull)
    implicit def attrDate(date: Date) = new AttributeValue().withS(dateFormat format date)
    implicit def attrString(ss: Set[String]) = new AttributeValue().withSS(ss)
    implicit def attrDouble(ns: Set[Double]) = new AttributeValue().withNS(ns map numberFormat)
    implicit def attrLong(ns: Set[Long]) = new AttributeValue().withNS(ns.map(_.toString))
    implicit class AttributeValueOpt(av: AttributeValue) {
      def getDate: Date = av.getS.option.map(dateFormat.parse).orNull
    }
    case class Column[A](name: String, getProp: T => A, valueOf: AttributeValue => A, toAttr: A => AttributeValue) {
      def apply(obj: T): (String, AttributeValue) = apply(getProp(obj))
      def apply(a: A): (String, AttributeValue) = name -> toAttr(a)
      def build(implicit map: Map[String, AttributeValue]): A = valueOf(map(name))
    }
    def columns: Set[Column[_]]
    def toAttributes(obj: T): Map[String, AttributeValue] = columns.map(_(obj)).toMap
    /**
     * Mapping to AttributeValues from Object
     */
    def fromMap(implicit map: Map[String, AttributeValue]): Option[T]
    /**
     * Column 'ID'
     */
    val id = Column[Long]("ID", (_.id), (_.getN.toLong), attrLong)
    /**
     * Column 'CREATED_AT'
     */
    val createdAt = Column[Date]("CREATED_AT", (_.createdAt), (_.getDate), attrDate)
    /**
     * Column 'LAST_MODIFIED_AT'
     */
    val lastModifiedAt = Column[Option[Date]]("LAST_MODIFIED_AT", (_.lastModifiedAt), (_.getDate.option), attrDateOpt)
    /**
     * Generating ID by random numbers.
     */
    def generateID: Long = {
      new Date().getTime + scala.util.Random.nextLong
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
    /**
     * Delete item.
     *
     * @return true if successfully done
     */
    def delete(i: Long): Boolean = {
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
    def get(i: Long): Option[T] = {
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
    def update(obj: T): Option[T] = update(obj.id, toAttributes(obj))
    def update(i: Long, attributes: (String, AttributeValue)*): Option[T] = update(i, attributes.toMap)
    def update(i: Long, attributes: Map[String, AttributeValue]): Option[T] = {
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
  }
}