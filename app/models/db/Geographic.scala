package models.db

import java.sql.Timestamp
import simple._
import scalaz._
import Scalaz._

case class Geographic(id: Long, equatorialRadius: Double, polarRadius: Double) {
  /**
   * Change property (like a copy) and update Database
   */
  def update(equatorialRadius: Double = equatorialRadius, polarRadius: Double = polarRadius): Option[Geographic] = {
    DB withSession { implicit session =>
      Geographics.filter(_.id is id).map { o =>
        (o.equatorialRadius, o.polarRadius)
      }.update((equatorialRadius, polarRadius)) == 1
    } option copy(equatorialRadius = equatorialRadius, polarRadius = polarRadius)
  }
}

class Geographics(tag: Tag) extends Table[Geographic](tag, "GEOGRAPHIC") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def equatorialRadius = column[Double]("EQUATORIAL_RADIUS", O.NotNull) // in meter
  def polarRadius = column[Double]("POLAR_RADIUS", O.NotNull) // in meter
  // All columns
  def * = (id, equatorialRadius, polarRadius) <> (Geographic.tupled, Geographic.unapply)
}
object Geographics extends TableQuery(new Geographics(_)) {
  def get = DB withSession { implicit session => this.first }
  /**
   * Add new
   */
  def addNew(equatorialRadius: Double, polarRadius: Double): Option[Geographic] = {
    val o = Geographic(-1, equatorialRadius, polarRadius)
    val newId = DB withSession { implicit session =>
      (this returning map(_.id)) += o
    }
    Option(newId) map { id => o.copy(id = id) }
  }
}
