
import scala.util.control.Exception._

package object models {
  implicit class HasAtt(xml: scala.xml.Node) {
    def \@(name: String) = (xml \ f"@$name").headOption.map(_.toString)
    def \@#(name: String) = \@(name).flatMap(v => allCatch opt v.toDouble)
  }
}