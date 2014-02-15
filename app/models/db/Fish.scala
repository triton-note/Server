package models.db

import java.sql.Timestamp
import simple._
import scalaz._
import Scalaz._

case class Fish(name: String, createdAt: Timestamp) {
  /**
   * Query for me
   */
  private def me = Fishes.filter(_.name is name)
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[Fish] = DB withSession { implicit session => me.firstOption }
  /**
   * Delete me
   */
  def delete: Boolean = DB.withSession { implicit session =>
    me.delete > 0
  }
}
class Fishes(tag: Tag) extends Table[Fish](tag, "FISH") {
  def name = column[String]("NAME", O.NotNull, O.PrimaryKey)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def * = (name, createdAt) <> (Fish.tupled, Fish.unapply)
}
object Fishes extends TableQuery(new Fishes(_)) {
  /**
   * Add new fish
   */
  def addNew(name: String): Option[Fish] = {
    val obj = Fish(name, currentTimestamp)
    DB withSession { implicit session =>
      (this += obj) == 1
    } option obj
  }
}

case class FishSize(id: Long,
                    fishName: String,
                    weight: Option[Double],
                    length: Option[Double]) {
  /**
   * Query for me
   */
  private def me = FishSizes.filter(_.id is id)
  /**
   * Reload from DB.
   * If there is no longer me, returns None.
   */
  def refresh: Option[FishSize] = DB withSession { implicit session => me.firstOption }
  /**
   * Delete me
   */
  def delete: Boolean = DB.withSession { implicit session =>
    me.delete > 0
  }
  /**
   * Fish
   */
  lazy val fish: Option[Fish] = DB withSession { implicit session =>
    me.flatMap(_.fish).firstOption
  }
}
class FishSizes(tag: Tag) extends Table[FishSize](tag, "FISH") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def fishName = column[String]("FISH_NAME", O.NotNull)
  def weight = column[Double]("WEIGHT", O.Nullable)
  def length = column[Double]("LENGTH", O.Nullable)
  def * = (id, fishName, weight.?, length.?) <> (FishSize.tupled, FishSize.unapply)
  /**
   * Bound fish
   */
  def fish = foreignKey("FISH_SIZE_FK_FISH", fishName, Fishes)(_.name)
}
object FishSizes extends TableQuery(new FishSizes(_)) {
  /**
   * Add new fish size
   */
  def addNew(fishName: String, weight: Option[Double] = None, length: Option[Double] = None): Option[FishSize] = {
    val obj = FishSize(-1, fishName, weight, length)
    val newId = DB withSession { implicit session =>
      (this returning map(_.id)) += obj
    }
    Option(newId) map { id => obj.copy(id = id) }
  }
}

