package models

import play.api.db.{ DB => PlayDB }
import play.api.Play.current
import scala.slick.session.Session
import scala.slick.driver.PostgresDriver.simple._

object DB {
  def now = new java.sql.Date(new java.util.Date().getTime)
  lazy val db = Database.forDataSource(PlayDB.getDataSource())
  def withSession[T](f: Session => T) = db.withSession(f)
  val prepared = {
    withSession { implicit session: Session =>
      def createTable(t: Table[_]) = {
        import scala.slick.jdbc.meta.MTable
        val table = MTable.getTables(t.tableName).firstOption
        if (table.isEmpty) t.ddl.create
      }
      List(
        User, Photo, Album,
        PhotoAlbum, PhotoOwner, AlbumOwner,
        Comment, PhotoComment, AlbumComment).foreach(createTable(_))
    }
  }
}