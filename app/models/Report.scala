package models

import java.util.Date

import scala.concurrent.Future

import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._

import com.amazonaws.services.dynamodbv2.document.KeyAttribute
import com.amazonaws.services.dynamodbv2.document.spec.QuerySpec

case class Report(
  id: String,
  userId: String,
  comment: Option[String],
  dateAt: Date,
  location: Report.Location,
  condition: Report.Condition,
  photo: Option[Photo],
  fishes: Seq[Report.Fishes]) {
  def save: Option[Report] = Report.save(this)
  def delete: Boolean = {
    Future(photo.foreach(_.delete))
    Report.delete(id)
  }
}
object Report {
  case class Location(name: String, geoinfo: GeoInfo)
  object Location {
    implicit val json = Json.format[Location]
  }
  case class Condition(moon: Int, tide: Condition.Tide.Value, weather: Option[Condition.Weather])
  object Condition {
    object Tide extends Enumeration {
      val FLOOD = Value("Flood")
      val HIGH = Value("High")
      val EBB = Value("Ebb")
      val LOW = Value("Low")
      implicit val json = Format[Value](
        Reads.verifying[String](values.map(_.toString).contains).map(withName),
        Writes { Json toJson _.toString })
    }
    case class Weather(nominal: String, temperature: ValueUnit.Temperature, iconUrl: String)
    object Weather {
      implicit val json = Json.format[Weather]
    }
    implicit val json = Json.format[Condition]
  }
  case class Fishes(
    name: String,
    count: Int,
    weight: Option[ValueUnit.Weight] = None,
    length: Option[ValueUnit.Length] = None) {
  }
  object Fishes {
    implicit val json = Json.format[Fishes]
  }
  implicit val json = Json.format[Report]

  /**
   *  Connect to DynamoDB Table
   */
  lazy val DB = new TableDelegate("REPORT")

  def save(given: Report): Option[Report] = {
    val report = {
      val fixComment = (a: Report) => {
        val v = a.comment.filter(_.length > 0)
        if (v == a.comment) a else a.copy(comment = v)
      }
      val fixId = (a: Report) => Option(a.id).filter(_.length > 0) match {
        case Some(_) => a
        case None    => a.copy(id = generateId)
      }
      val fixSpotName = (a: Report) => Option(a.location.name).filter(_.length > 0) match {
        case Some(_) => a
        case None    => a.copy(location = a.location.copy(name = " "))
      }
      (fixId compose fixComment compose fixSpotName)(given)
    }
    DB.save(report)(_
      .withString("USER_ID", report.userId)
      .withNumber("DATE_AT", report.dateAt.getTime))
  }
  def get(id: String): Option[Report] = DB get id
  def delete(id: String): Boolean = DB delete id
  /**
   * 特定のユーザの Report を指定された数だけ取り出す。
   * 前回の最後の Report の id を指定するとその次から取り出す。
   * limit に 0 を指定するとすべて取り出す。
   */
  def findBy(userId: String, limit: Int = 0, last: Option[Report] = None): Stream[Report] = {
    DB.query("USER_ID-DATE_AT-index")(
      { s: QuerySpec =>
        if (limit < 1) s else {
          Logger trace f"Paging ${DB.TABLE.getTableName}: pageSize: ${limit}: last: ${last}"
          s.withMaxResultSize(limit)
        }
      } andThen { s =>
        last match {
          case None => s
          case Some(report) => s.withExclusiveStartKey(
            new KeyAttribute("ID", report.id),
            new KeyAttribute("USER_ID", report.userId),
            new KeyAttribute("DATE_AT", new java.lang.Long(report.dateAt.getTime))
          )
        }
      } andThen (_.withHashKey("USER_ID", userId))
    )
  }
}
