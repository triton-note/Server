package models

import scala.util.control.Exception._

object RichXML {
  implicit def HasAtt(xml: scala.xml.Node) = new {
    def \@(name: String) = (xml \ f"@$name").headOption.map(_.toString)
    def \@%(name: String) = \@(name).flatMap(v => allCatch opt v.toDouble)
    def \@#(name: String) = \@(name).flatMap(v => allCatch opt v.toInt)
  }
}