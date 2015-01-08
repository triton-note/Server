package service

import java.util.Date

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import models.GeoInfo
import models.Report.Condition.Tide
import models.Report.Condition.Weather

/**
 * TODO NaturalConditions
 */
object NaturalConditions {
  def moon(datetime: Date, geoinfo: GeoInfo): Future[Int] = Future {
    0
  }
  def tide(datetime: Date, geoinfo: GeoInfo): Future[Tide.Value] = Future {
    Tide.FLOOD
  }
  def weather(datetime: Date, geoinfo: GeoInfo): Future[Weather] = Future {
    Weather("Fine", 20.0)
  }
}
