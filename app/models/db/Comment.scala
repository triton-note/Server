package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession

case class Comment(id: Long,
                   createdAt: Timestamp,
                   lastModifiedAt: Option[Timestamp],
                   userId: Long,
                   text: String) {
  lazy val user = withSession {
    val q = for {
      a <- me
      b <- a.user
    } yield b
    q.first
  }
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
    withSession {
      me.delete
    }
  }
  /**
   * Change text
   */
  def update(theText: String): Comment = {
    val n = copy(lastModifiedAt = Some(currentTimestamp), text = theText)
    withSession {
      me.map { a =>
        (a.lastModifiedAt.? ~ a.text)
      }.update(n.lastModifiedAt, n.text)
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
  def * = id ~ createdAt ~ lastModifiedAt.? ~ userId ~ text <> (Comment.apply _, Comment.unapply _)
  /**
   * Bound user
   */
  def user = foreignKey("COMMENT_FK_USER", userId, User)(_.id)
  /**
   * Add new comment
   */
  def addNew(theUser: User, theText: String): Comment = {
    val now = currentTimestamp
    val newId = withSession {
      def p = createdAt ~ userId ~ text
      p returning id insert (now, theUser.id, theText)
    }
    Comment(newId, now, None, theUser.id, theText)
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
    withTransaction {
      * insert (theComment.id, theAlbum.id)
      val q = for {
        a <- Comment
        b <- Album
        if (a.id === theComment.id)
        if (b.id === theAlbum.id)
      } yield (a, b)
      q.first
    }
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
    withTransaction {
      * insert (theComment.id, thePhoto.id)
      val q = for {
        a <- Comment
        b <- Photo
        if (a.id === theComment.id)
        if (b.id === thePhoto.id)
      } yield (a, b)
      q.first
    }
  }
}
