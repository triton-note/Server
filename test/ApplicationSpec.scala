package test

import org.specs2._

import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 * For more information, consult the wiki.
 */
class ApplicationSpec extends Specification {
  def is = s2"""
  異常な Request には 404 を返す             $e1
  トップページはない                         $t1
  
  """

  def e1 = running(FakeApplication()) {
    route(FakeRequest(GET, "/boum")) must beNone
  }
  def t1 = running(FakeApplication()) {
    route(FakeRequest(GET, "/")) must beNone
  }
}