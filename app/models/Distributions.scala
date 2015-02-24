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

  def catches(userId: Option[String], limit: Int = 100): Stream[Catch] = {
    def mkStream(p: Option[String] => Stream[Report]): Stream[Report] = {
      p(None)
    }
    for {
      report <- userId match {
        case None    => mkStream { Report.DB.paging(limit, _) }
        case Some(u) => mkStream { Report.findBy(u, limit, _) }
      }
      fish <- report.fishes
    } yield Catch(
      userId.map(_ => report.id),
      fish.monaker,
      fish.quantity,
      report.dateAt,
      report.location.geoinfo)
  }
  def names: Stream[NameCount] = {
    val fishes = catches(None)
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
