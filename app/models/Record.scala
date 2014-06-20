package models

import play.api.libs.json.{ JsValue, Json }
import play.api.libs.json.Json.toJsFieldJsValueWrapper

case class Record(
  comment: String,
  location: String,
  geoinfo: Option[GeoInfo],
  catches: Seq[Record.Catches]) {

  def toJson = Json.obj(
    "comment" -> comment,
    "location" -> location,
    "geoinfo" -> geoinfo.map(_.toJson),
    "catches" -> catches.map(_.toJson)
  )
}
object Record {
  case class Catches(
    name: String,
    count: Int,
    weight: Option[Double],
    length: Option[Double]) {

    def toJson = Json.obj(
      "name" -> name,
      "count" -> count,
      "weight" -> weight,
      "length" -> length
    )
  }
  object Catches {
    def fromJson(jv: JsValue) = {
      for {
        name <- (jv \ "name").asOpt[String]
        count <- (jv \ "count").asOpt[Int]
      } yield {
        val weight = (jv \ "weight").asOpt[Double]
        val length = (jv \ "length").asOpt[Double]
        Catches(name, count, weight, length)
      }
    }
  }

  def fromJson(jv: JsValue) = {
    for {
      comment <- (jv \ "comment").asOpt[String]
      location <- (jv \ "location").asOpt[String]
    } yield {
      val geoinfo = (jv \\ "geoinfo").map(GeoInfo.fromJson).flatten.headOption
      val catches = (jv \\ "catches").map(Catches.fromJson).flatten
      Record(comment, location, geoinfo, catches)
    }
  }
}