package models

import java.sql.Date
import scala.slick.driver.PostgresDriver.simple._

object Comment extends Table[(Long, String, Option[String])]("COMMENT") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def text = column[String]("text", O.Default(""))
  def user = column[String]("user")
  def * = id ~ text ~ user.?
  def fkUser = foreignKey("user_fk", user, User)(_.account)
}

object AlbumComment extends Table[(Long, Long)]("ALBUM_COMMENT") {
  def album = column[Long]("album", O.NotNull)
  def comment = column[Long]("comment", O.NotNull)
  def * = album ~ comment
  def fkAlbum = foreignKey("album_fk", album, Album)(_.id)
  def fkComment = foreignKey("comment_fk", comment, Comment)(_.id)
}

object PhotoComment extends Table[(Long, Long)]("PHOTO_COMMENT") {
  def photo = column[Long]("photo", O.NotNull)
  def comment = column[Long]("comment", O.NotNull)
  def * = photo ~ comment
  def fkPhoto = foreignKey("photo_fk", photo, Photo)(_.id)
  def fkComment = foreignKey("comment_fk", comment, Comment)(_.id)
}
