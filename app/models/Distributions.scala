package models

import java.util.Date

import scala.annotation.tailrec
import scala.collection.JavaConversions._

import play.api.libs.json._

object Distributions {
  case class Catch(
    reportId: Option[String],
    monaker: String,
    quantity: Int,
    dateAt: Date,
    geoinfo: GeoInfo)
  object Catch {
    implicit val json = Json.format[Catch]
  }
  case class NameCount(monaker: String, quantity: Int)
  object NameCount {
    implicit val json = Json.format[NameCount]
  }

  def catches(userId: Option[String]): Stream[Catch] = {
    for {
      report <- userId match {
        case None    => Report.DB.stream()()
        case Some(u) => Report.findBy(u, 0, None)
      }
      fish <- report.fishes
    } yield Catch(
      userId.map(_ => report.id),
      fish.monaker,
      fish.quantity,
      report.dateAt,
      report.location.geoinfo)
  }
  def monakers: Stream[NameCount] = {
    val fishes = catches(None)
    @tailrec
    def countUp(list: Stream[Catch], counter: Map[String, Int] = Map()): Map[String, Int] = {
      if (list.isEmpty) counter
      else {
        val monaker = list.head.monaker
        val quantity = ((counter get monaker) getOrElse 0) + 1
        countUp(list.tail, counter + (monaker -> quantity))
      }
    }
    for {
      (monaker, quantity) <- countUp(fishes).toStream
    } yield NameCount(monaker, quantity)
  }
}
