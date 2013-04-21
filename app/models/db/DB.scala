package models.db

import play.api.db.{ DB => PlayDB }
import play.api.Play.current
import scala.slick.jdbc.meta.MTable

object DB {
  val simple = scala.slick.driver.PostgresDriver.simple
  import simple._
  import Database.threadLocalSession
  lazy val db = Database.forDataSource(PlayDB.getDataSource())
  val prepared = {
    db withSession {
      def createTable(t: Table[_]) = {
        import scala.slick.jdbc.meta.MTable
        val table = MTable.getTables(t.tableName).firstOption
        if (table.isEmpty) t.ddl.create
      }
      List(
        User, Photo, Album,
        UserAlias, VolatileToken,
        PhotoAlbum, PhotoOwner, AlbumOwner,
        Comment, CommentPhoto, CommentAlbum).foreach(createTable(_))
    }
  }
  def withSession[T](f: => T) = db.withSession(f)
  def withTransaction[T](f: => T) = db.withTransaction(f)
  def now = new java.sql.Timestamp(new java.util.Date().getTime)
}