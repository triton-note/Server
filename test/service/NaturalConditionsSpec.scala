package service

import java.util.Date

import scala.concurrent.duration._

import play.api.Logger
import play.api.test.FakeApplication
import play.api.test.Helpers.running

import org.fathens.math._
import org.scalacheck._
import org.specs2._

import models.Report.Condition.Tide

object NaturalConditionsSpec extends Specification with ScalaCheck {
  def is = s2"""
  TideState

    according to Moon                $ts01

  Weather

    current info at Athens           $ci01
    past info at Tokyo               $ci02
"""

  implicit val genDegrees = Arbitrary(Arbitrary.arbitrary[Double].map(Degrees).map(_.normalize: Degrees))

  def ts01 = prop { (origin: Degrees, moon: Degrees) =>
    import models.Report.Condition.Tide
    val actual = NaturalConditions.tideState(origin, moon)
    val diff: Degrees = (moon - origin).normalize
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

  def ci01 = running(FakeApplication())  {
    val geoinfo = models.GeoInfo(Degrees(37.9908372), Degrees(23.7383394))
    NaturalConditions.weather(new Date, geoinfo).map { weather =>
      Logger debug f"${weather} at ${geoinfo}"
      weather must beSome
    }.await(3, FiniteDuration(10, MINUTES))
  }
  def ci02 = running(FakeApplication())  {
    val geoinfo = models.GeoInfo(Degrees(35.681710), Degrees(139.748154))
    val date = new Date(new Date().getTime - 24 * 60 * 60 * 1000)
    NaturalConditions.weather(date, geoinfo).map { weather =>
      Logger debug f"${weather} at ${geoinfo}"
      weather must beSome
    }.await(3, FiniteDuration(10, MINUTES))
  }
}
