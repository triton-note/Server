package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession

case class Comment(id: Long,
                   createdAt: Timestamp,
                   lastModifiedAt: Option[Timestamp],
                   user: User,
                   text: String) {
  /**
   * Prepared query for me
   */
  lazy val me = for {
    a <- Comment
    if (a.id === id)
  } yield a
  /**
   * Delete me
   */
  def delete = {
    DB withSession {
      me.delete
    }
  }
  /**
   * Change property (like a copy) and update Database
   */
  def update(theUser: User = user, theText: String = text): Comment = {
    val n = copy(lastModifiedAt = Some(DB.now), user = theUser, text = theText)
    DB.withSession {
      me.map { a =>
        (a.lastModifiedAt.? ~ a.userId ~ a.text)
      }.update(n.lastModifiedAt, n.user.id, n.text)
    }
    n
  }
}

object Comment extends Table[Comment]("COMMENT") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def userId = column[Long]("USER", O.NotNull)
  def text = column[String]("TEXT", O.NotNull, O.Default(""))
  // All columns
  def * = id ~ createdAt ~ lastModifiedAt.? ~ userId ~ text <> (
    { t => Comment(t._1, t._2, t._3, User.get(t._4).get, t._5) },
    { o => Some(o.id, o.createdAt, o.lastModifiedAt, o.user.id, o.text) })
  /**
   * Bound user
   */
  def user = foreignKey("COMMENT_FK_USER", userId, User)(_.id)
  /**
   * Add new comment
   */
  def addNew(theUser: User, theText: String): Comment = {
    val now = DB.now
    val newId = DB withSession {
      def p = createdAt ~ userId ~ text
      p returning id insert (now, theUser.id, theText)
    }
    Comment(newId, now, None, theUser, theText)
  }
}

object CommentAlbum extends Table[(Long, Long)]("ALBUM_COMMENT") {
  def commentId = column[Long]("COMMENT", O.NotNull)
  def albumId = column[Long]("ALBUM", O.NotNull)
  // All columns
  def * = commentId ~ albumId
  /**
   * Bound album
   */
  def album = foreignKey("ALBUM_COMMENT_FK_ALBUM", albumId, Album)(_.id)
  /**
   * Bound comment
   */
  def comment = foreignKey("ALBUM_COMMENT_FK_COMMENT", commentId, Comment)(_.id)
  /**
   * Add new comment to album
   */
  def addNew(theComment: Comment, theAlbum: Album): (Comment, Album) = {
    DB withSession {
      * insert (theComment.id, theAlbum.id)
    }
    (theComment, theAlbum)
  }
}

object CommentPhoto extends Table[(Long, Long)]("PHOTO_COMMENT") {
  def commentId = column[Long]("COMMENT", O.NotNull)
  def photoId = column[Long]("PHOTO", O.NotNull)
  // All columns
  def * = commentId ~ photoId
  /**
   * Bound photo
   */
  def photo = foreignKey("PHOTO_COMMENT_FK_PHOTO", photoId, Photo)(_.id)
  /**
   * Bound comment
   */
  def comment = foreignKey("PHOTO_COMMENT_FK_COMMENT", commentId, Comment)(_.id)
  /**
   * Add new comment to photo
   */
  def addNew(theComment: Comment, thePhoto: Photo): (Comment, Photo) = {
    DB withSession {
      * insert (theComment.id, thePhoto.id)
    }
    (theComment, thePhoto)
  }
}
