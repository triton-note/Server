package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession

case class Album(id: Long,
                 createdAt: Timestamp,
                 lastModifiedAt: Option[Timestamp],
                 date: Timestamp,
                 grounds: String) {
  lazy val photos = withSession {
    val q = for {
      a <- me
      pa <- PhotoAlbum
      if (pa.albumId === a.id)
      p <- Photo
    } yield p
    q.list
  }
  /**
   * Prepared query for me
   */
  lazy val me = for {
    a <- Album
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
   * Change property (like a copy) and update Database
   */
  def update(date: Timestamp = date, grounds: String = grounds): Album = {
    val n = copy(lastModifiedAt = Some(currentTimestamp), date = date, grounds = grounds)
    withSession {
      me.map { a =>
        (a.lastModifiedAt.? ~ a.date ~ a.grounds)
      }.update(n.lastModifiedAt, n.date, n.grounds)
    }
    n
  }
}

object Album extends Table[Album]("ALBUM") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def date = column[Timestamp]("DATE", O.NotNull)
  def grounds = column[String]("GROUNDS", O.NotNull)
  // All columns
  def * = id ~ createdAt ~ lastModifiedAt.? ~ date ~ grounds <> (Album.apply _, Album.unapply _)
  /**
   * Add new album
   */
  def addNew(theDate: Timestamp, theGrounds: String): Album = {
    val timestamp = currentTimestamp
    val newId = withSession {
      def p = createdAt ~ date ~ grounds
      p returning id insert (timestamp, theDate, theGrounds)
    }
    Album(newId, timestamp, None, theDate, theGrounds)
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
    withSession {
      * insert (photo.id, album.id)
    }
    (photo, album)
  }
}
