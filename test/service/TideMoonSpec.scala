package service

import org.fathens.math._
import org.scalacheck._
import org.specs2._

object TideMoonSpec extends Specification with ScalaCheck {
  def is = s2"""
  TideState

    according to Moon                $ts01
"""

  implicit val genDegrees = Arbitrary(Arbitrary.arbitrary[Double].map(Degrees).map(_.normalize: Degrees))

  def ts01 = prop { (origin: Degrees, moon: Degrees) =>
    import TideMoon._
    val actual = TideState.of(origin, moon)
    val diff: Degrees = (moon- origin).normalize
    val expected = diff.toDouble match {
      case d if d < 15   => TideState.High
      case d if d <= 75  => TideState.Flood
      case d if d < 105  => TideState.Low
      case d if d <= 165 => TideState.Ebb
      case d if d < 195  => TideState.High
      case d if d <= 255 => TideState.Flood
      case d if d < 285  => TideState.Low
      case d if d <= 345 => TideState.Ebb
      case d if d < 360  => TideState.High
    }
    actual must_== expected
  }
}
