package models

import java.util.Date

import scala.collection.JavaConversions._

import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

import service.{ Settings, Storage }

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
  case class Location(name: String, geoinfo: GeoInfo)
  object Location {
    implicit val json = Json.format[Location]
  }
  case class Condition(moon: Int, tide: Condition.Tide.Value, weather: Option[Condition.Weather])
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
    case class Weather(nominal: String, temperature: ValueUnit.Temperature, iconUrl: String)
    object Weather {
      implicit val json = Json.format[Weather]
    }
    implicit val json = Json.format[Condition]
  }
  case class Photo(
    original: Photo.Image,
    mainview: Photo.Image,
    thumbnail: Photo.Image)
  object Photo {
    case class Image(file: Storage.S3File)
    object Image {
      implicit val json = Format[Image](
        (__ \ "path").read[String].map(Storage file _).map(Image.apply),
        Writes { image =>
          Json.obj(
            "path" -> image.file.path,
            "volatileUrl" -> image.file.generateURL(Settings.Image.urlExpiration).toString
          )
        }
      )
      object Kind extends Enumeration {
        val ORIGINAL = Value("original")
        val REDUCED = Value("reduced")
      }
    }
    implicit val json = Json.format[Photo]
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
  /**
   * 特定のユーザの Report を指定された数だけ取り出す。
   * 前回の最後の Report の id を指定するとその次から取り出す。
   * count に 0 を指定するとすべて取り出す。
   */
  def findBy(userId: String, count: Int, last: Option[String]): Stream[Report] = {
    val scaner = if (count < 1) DB.stream()_ else DB.paging(count, last)_
    scaner(_.withFilterExpression(f"${DB.json("userId")} = :user").withValueMap(Map(":user" -> userId)))
  }
}
