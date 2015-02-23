package models

import java.util.Date

import scala.annotation.tailrec
import scala.collection.JavaConversions._

import play.api.libs.json._

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

  def catches(userOption: Option[User], limit: Int = 100): Stream[Catch] = {
    def others = Report.DB.scan()
    def byUser(user: User) = Report.DB.scan(_.withFilterExpression(""))
    for {
      report <- userOption match {
        case None    => others
        case Some(u) => byUser(u)
      }
      fish <- report.fishes
    } yield Catch(
      userOption.map(_ => report.id),
      fish.name,
      fish.count,
      report.dateAt,
      report.location.geoinfo)
  }
  def names(limit: Int = 100): Stream[NameCount] = {
    val fishes = catches(None, limit)
    @tailrec
    def countUp(list: Stream[Catch], counter: Map[String, Int] = Map()): Map[String, Int] = {
      if (list.isEmpty) counter
      else {
        val name = list.head.name
        val count = ((counter get name) getOrElse 0) + 1
        countUp(list.tail, counter + (name -> count))
      }
    }
    for {
      (name, count) <- countUp(fishes).toStream
    } yield NameCount(name, count)
  }
}
