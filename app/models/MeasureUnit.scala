package models

import play.api.libs.json._

object MeasureUnit {
  object Length extends Enumeration {
    val CM = Value("cm")
    val INCH = Value("inch")
    implicit val json = Format(
      (__).read[String].map(Length.withName),
      Writes { (t: Length.Value) => JsString(t.toString) })
  }
  object Weight extends Enumeration {
    val KG = Value("kg")
    val POND = Value("pond")
    implicit val json = Format(
      (__).read[String].map(Weight.withName),
      Writes { (t: Weight.Value) => JsString(t.toString) })
  }
  object Temperature extends Enumeration {
    val Cels = Value("℃")
    val Fahr = Value("℉")
    implicit val json = Format(
      (__).read[String].map(Temperature.withName),
      Writes { (t: Temperature.Value) => JsString(t.toString) })
  }
  implicit val json = Json.format[MeasureUnit]
}
case class MeasureUnit(length: MeasureUnit.Length.Value, weight: MeasureUnit.Weight.Value, temperature: MeasureUnit.Temperature.Value) {
  lazy val asJson = Json toJson this
}
