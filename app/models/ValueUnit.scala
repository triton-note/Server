package models

import play.api.libs.json._

trait ValueUnit[U] {
  val numValue: Double
  val measure: U
  override def toString: String = f"${numValue}%1.1f${measure}"
}
object ValueUnit {
  case class Length(numValue: Double, measure: ValueUnit.Length.Measure.Value) extends ValueUnit[ValueUnit.Length.Measure.Value]
  object Length {
    object Measure extends Enumeration {
      val CM = Value("cm")
      val INCH = Value("inch")
      implicit val json = Format[Measure.Value](
        (__).read[String].map(Measure.withName),
        Writes { Json toJson _.toString })
    }
    implicit val json = Json.format[Length]
  }
  case class Weight(numValue: Double, measure: ValueUnit.Weight.Measure.Value) extends ValueUnit[ValueUnit.Weight.Measure.Value]
  object Weight {
    object Measure extends Enumeration {
      val KG = Value("kg")
      val POND = Value("pond")
      implicit val json = Format[Measure.Value](
        (__).read[String].map(Measure.withName),
        Writes { Json toJson _.toString })
    }
    implicit val json = Json.format[Weight]
  }
  case class Temperature(numValue: Double, measure: ValueUnit.Temperature.Measure.Value) extends ValueUnit[ValueUnit.Temperature.Measure.Value]
  object Temperature {
    object Measure extends Enumeration {
      val Cels = Value("Cels")
      val Fahr = Value("Fahr")
      implicit val json = Format[Measure.Value](
        (__).read[String].map(Measure.withName),
        Writes { Json toJson _.toString })
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
