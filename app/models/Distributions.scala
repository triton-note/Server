package models

import java.util.Date

import scala.annotation.tailrec
import scala.collection.JavaConversions._

import play.api.libs.json._

import com.amazonaws.services.dynamodbv2.model._

import models.db.{CatchReport, FishSize, Photo, TableRoot, User}
import service.AWS.DynamoDB.client

object Distributions {
  case class Catch(
    reportId: Option[String],
    name: String,
    count: Int,
    date: Date,
    geoinfo: GeoInfo)
  object Catch {
    implicit val catchFormat = Json.format[Catch]
  }
  case class NameCount(name: String, count: Int)
  object NameCount {
    implicit val nameCountFormat = Json.format[NameCount]
  }

  type Result = {
    def getItems(): java.util.List[java.util.Map[String, AttributeValue]]
    def getLastEvaluatedKey(): java.util.Map[String, AttributeValue]
  }
  def making[A](table: TableRoot[A])(listUp: Option[java.util.Map[String, AttributeValue]] => Result) = {
    def addHead[A](first: Seq[A], next: => Stream[A]): Stream[A] = {
      if (first.isEmpty) next
      else first.head #:: addHead(first.tail, next)
    }
    def con(last: Option[java.util.Map[String, AttributeValue]] = None): Stream[A] = {
      val result = listUp(last)
      val list = result.getItems.toStream.map(_.toMap).map(table.apply)
      Option(result.getLastEvaluatedKey) match {
        case None      => addHead(list, Stream.Empty)
        case Some(key) => addHead(list, con(Some(key)))
      }
    }
    con()
  }
  def catches(userOption: Option[User], limit: Int = 100): Seq[Catch] = {
    def others: Stream[CatchReport] = making(CatchReport) { last =>
      client.scan(new ScanRequest(CatchReport.tableName).withLimit(limit).withExclusiveStartKey(last.orNull))
    }
    def byUser(user: User): Stream[CatchReport] = making(CatchReport) { last =>
      client.query(new QueryRequest(CatchReport.tableName).withLimit(limit).withExclusiveStartKey(last.orNull).
        withIndexName("USER-TIMESTAMP-index").
        withKeyConditions(Map(
          CatchReport.user compare Option(user)
        )))
    }
    for {
      report <- userOption match {
        case None    => others
        case Some(u) => byUser(u)
      }
      photo <- Photo.findBy(report)
      fish <- FishSize.findBy(photo)
    } yield Catch(
      userOption.map(_ => report.id),
      fish.name,
      fish.count.toInt,
      report.timestamp,
      report.geoinfo)
  }
  def names(limit: Int = 100): Seq[NameCount] = {
    val fishes = making(FishSize) { last =>
      client.scan(new ScanRequest(FishSize.tableName).withLimit(limit).withExclusiveStartKey(last.orNull))
    }
    @tailrec
    def countUp(list: Stream[FishSize], counter: Map[String, Int] = Map()): Map[String, Int] = {
      if (list.isEmpty) counter
      else {
        val name = list.head.name
        val count = ((counter get name) getOrElse 0) + 1
        countUp(list.tail, counter + (name -> count))
      }
    }
    for {
      (name, count) <- countUp(fishes).toList
    } yield NameCount(name, count)
  }
}
