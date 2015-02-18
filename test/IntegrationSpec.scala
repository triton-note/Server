package test

import org.specs2._

import play.api.test._
import play.api.test.Helpers._

/**
 * add your integration spec here.
 * An integration test will fire up a whole play application in a real (or headless) browser
 */
class IntegrationSpec extends Specification {
  def is = s2"""
  Application work from with in a browser $a1
"""
  def a1 = {
    running(TestServer(3333), HTMLUNIT) { browser =>
      browser.goTo("http://localhost:3333/boum")
      browser.pageSource must contain("Action not found")
    }
  }
}