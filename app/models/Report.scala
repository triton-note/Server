package models

import java.util.Date
import scala.collection.JavaConversions._

import play.api.libs.json._

import service.Storage.S3File

case class Report(
  id: String,
  userId: String,
  comment: Option[String],
  dateAt: Date,
  location: Report.Location,
  naturalCondition: Report.Condition,
  photo: Option[Report.Photo],
  fishes: Seq[Report.Fishes]) {
  def save: Option[Report] = Report.save(this)
  def delete: Boolean = Report.delete(id)
}
object Report {
  case class Location(monaker: String, geoinfo: GeoInfo)
  object Location {
    implicit val locationFormat = Json.format[Location]
  }
  case class Condition(moon: Int, tide: Condition.Tide.Value, weather: Option[Condition.Weather]) {
    lazy val asJson = Json toJson this
  }
  object Condition {
    object Tide extends Enumeration {
      val Flood = Value("Flood")
      val High = Value("High")
      val Ebb = Value("Ebb")
      val Low = Value("Low")
      implicit val tideFormat = Format(
        (__).read[String].map(Tide.withName),
        Writes { (t: Tide.Value) => JsString(t.toString) })
    }
    case class Weather(monaker: String, temperature: ValueUnit.Temperature, iconUrl: String)
    object Weather {
      implicit val weatherFormat = Json.format[Weather]
    }
    implicit val json = Json.format[Condition]
  }
  case class Photo(
    original: S3File,
    mainview: S3File,
    thumbnail: S3File)
  object Photo {
    implicit val photoFormat = Json.format[Photo]

    object Kind extends Enumeration {
      val ORIGINAL = Value("original")
      val REDUCED = Value("reduced")
    }
  }
  case class Fishes(
    monaker: String,
    quantity: Int,
    sizeWeight: Option[ValueUnit.Weight] = None,
    sizeLength: Option[ValueUnit.Length] = None) {
  }
  object Fishes {
    implicit val catchesFormat = Json.format[Fishes]
  }
  implicit val reportFormat = Json.format[Report]

  /**
   *  Connect to DynamoDB Table
   */
  lazy val DB = new TableDelegate("REPORT")

  def save(report: Report): Option[Report] = {
    val fixComment = (a: Report) => {
      val v = a.comment.filter(_.length > 0)
      if (v == a.comment) a else a.copy(comment = v)
    }
    val fixId = (a: Report) => {
      val v = Option(a.id).filter(_.length > 0) getOrElse generateId
      if (v == a.id) a else a.copy(id = v)
    }
    DB save (fixId compose fixComment)(report)
  }
  def get(id: String): Option[Report] = DB get id
  def delete(id: String): Boolean = DB delete id
  def findBy(userId: String, count: Int, last: Option[String]): Stream[Report] = {
    val values = Map(":user" -> userId)
    DB.paging(count, last)(_.withFilterExpression(f"${DB.json("userId")} = :user").withValueMap(values))
  }
}
