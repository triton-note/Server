package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession

case class Album(id: Long,
                 createdAt: Timestamp,
                 lastModifiedAt: Option[Timestamp],
                 date: Option[Timestamp],
                 grounds: Option[String]) {
  /**
   * Prepared query for me
   */
  val me = for {
    a <- Album
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
  def update(date: Option[Timestamp] = date, grounds: Option[String] = grounds): Album = {
    val n = copy(lastModifiedAt = Some(DB.now), date = date, grounds = grounds)
    DB.withSession {
      me.map { a =>
        (a.lastModifiedAt.? ~ a.date.? ~ a.grounds.?)
      }.update(n.lastModifiedAt, n.date, n.grounds)
    }
    n
  }
}

object Album extends Table[Album]("ALBUM") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def date = column[Timestamp]("DATE", O.Nullable)
  def grounds = column[String]("GROUNDS", O.Nullable)
  // All columns
  def * = id ~ createdAt ~ lastModifiedAt.? ~ date.? ~ grounds.? <> (Album.apply _, Album.unapply _)
  /**
   * Add new album
   */
  def addNew(theDate: Option[Timestamp], theGrounds: Option[String]): Album = {
    val now = DB.now
    val newId = DB withSession {
      def p = createdAt ~ date.? ~ grounds.?
      p returning id insert (now, theDate, theGrounds)
    }
    Album(newId, now, None, theDate, theGrounds)
  }
}

object PhotoAlbum extends Table[(Long, Long)]("PHOTO_ALBUM") {
  def photoId = column[Long]("PHOTO", O.NotNull)
  def albumId = column[Long]("ALBUM", O.NotNull)
  // All columns
  def * = photoId ~ albumId
  /**
   * Bound photo
   */
  def photo = foreignKey("PHOTO_ALBUM_FK_PHOTO", photoId, Photo)(_.id)
  /**
   * Bound album
   */
  def album = foreignKey("PHOTO_ALBUM_FK_ALBUM", albumId, Album)(_.id)
  /**
   * Let album gain photo
   */
  def addNew(photo: Photo, album: Album): (Photo, Album) = {
    val ai = DB withSession {
      * returning albumId insert (photo.id, album.id)
    }
    assert(ai == album.id)
    (photo, album)
  }
}