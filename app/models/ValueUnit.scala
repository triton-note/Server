package models

import play.api.libs.json._

trait ValueUnit[U] {
  val value: Double
  val unit: U
  override def toString: String = f"${value}%1.1f${unit}"
}
object ValueUnit {
  case class Length(value: Double, unit: ValueUnit.Length.Measure.Value) extends ValueUnit[ValueUnit.Length.Measure.Value]
  object Length {
    object Measure extends Enumeration {
      val CM = Value("cm")
      val INCH = Value("inch")
      implicit val json = Format(
        (__).read[String].map(Measure.withName),
        Writes { (t: Measure.Value) => JsString(t.toString) })
    }
    implicit val json = Json.format[Length]
  }
  case class Weight(value: Double, unit: ValueUnit.Weight.Measure.Value) extends ValueUnit[ValueUnit.Weight.Measure.Value]
  object Weight {
    object Measure extends Enumeration {
      val KG = Value("kg")
      val POND = Value("pond")
      implicit val json = Format(
        (__).read[String].map(Measure.withName),
        Writes { (t: Measure.Value) => JsString(t.toString) })
    }
    implicit val json = Json.format[Weight]
  }
  case class Temperature(value: Double, unit: ValueUnit.Temperature.Measure.Value) extends ValueUnit[ValueUnit.Temperature.Measure.Value]
  object Temperature {
    object Measure extends Enumeration {
      val Cels = Value("Cels")
      val Fahr = Value("Fahr")
      implicit val json = Format(
        (__).read[String].map(Measure.withName),
        Writes { (t: Measure.Value) => JsString(t.toString) })
    }
    implicit val json = Json.format[Temperature]
  }

  object Measures {
    implicit val json = Json.format[Measures]
  }
  case class Measures(length: ValueUnit.Length.Measure.Value, weight: ValueUnit.Weight.Measure.Value, temperature: ValueUnit.Temperature.Measure.Value) {
    lazy val asJson = Json toJson this
  }
}
