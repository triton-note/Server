package service

import org.fathens.math._
import org.scalacheck._
import org.specs2._

object NaturalConditionsSpec extends Specification with ScalaCheck {
  def is = s2"""
  TideState

    according to Moon                $ts01
"""

  implicit val genDegrees = Arbitrary(Arbitrary.arbitrary[Double].map(Degrees).map(_.normalize: Degrees))

  def ts01 = prop { (origin: Degrees, moon: Degrees) =>
    import models.Report.Condition.Tide
    val actual = NaturalConditions.tideState(origin, moon)
    val diff: Degrees = (moon- origin).normalize
    val expected = diff.toDouble match {
      case d if d < 15   => Tide.High
      case d if d <= 75  => Tide.Flood
      case d if d < 105  => Tide.Low
      case d if d <= 165 => Tide.Ebb
      case d if d < 195  => Tide.High
      case d if d <= 255 => Tide.Flood
      case d if d < 285  => Tide.Low
      case d if d <= 345 => Tide.Ebb
      case d if d < 360  => Tide.High
    }
    actual must_== expected
  }
}
