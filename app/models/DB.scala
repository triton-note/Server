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
      (User.ddl ++ Photo.ddl ++ Album.ddl).create
      (PhotoAlbum.ddl ++ PhotoOwner.ddl ++ AlbumOwner.ddl).create
      (Comment.ddl ++ PhotoComment.ddl ++ AlbumComment.ddl).create
    }
  }
}