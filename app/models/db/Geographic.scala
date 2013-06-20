package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession

case class Geographic(id: Long, equatorialRadius: Double, polarRadius: Double) {
  /**
   * Change property (like a copy) and update Database
   */
  def update(equatorialRadius: Double = equatorialRadius, polarRadius: Double = polarRadius): Geographic = {
    val n = copy(equatorialRadius = equatorialRadius, polarRadius = polarRadius)
    withSession {
      val q = for {
        a <- Geographic
        if (a.id === id)
      } yield (a.equatorialRadius ~ a.polarRadius)
      q.update(n.equatorialRadius, n.polarRadius)
    }
    n
  }
}

object Geographic extends Table[Geographic]("GEOGRAPHIC") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def equatorialRadius = column[Double]("EQUATORIAL_RADIUS", O.NotNull) // in meter
  def polarRadius = column[Double]("POLAR_RADIUS", O.NotNull) // in meter
  // All columns
  def * = id ~ equatorialRadius ~ polarRadius <> (Geographic.apply _, Geographic.unapply _)
  def get = withSession {
    val q = for {
      a <- Geographic
    } yield a
    q.first
  }
  /**
   * Add new album
   */
  def addNew(theEquatorialRadius: Double, thePolarRadius: Double): Geographic = {
    val newId = withSession {
      def p = equatorialRadius ~ polarRadius
      p returning id insert (theEquatorialRadius, thePolarRadius)
    }
    Geographic(newId, theEquatorialRadius, thePolarRadius)
  }
}
