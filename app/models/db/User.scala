package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession
import com.amazonaws.util.Md5Utils

case class User(id: Long,
                createdAt: Timestamp = DB.now,
                lastModifiedAt: Option[Timestamp] = None,
                firstName: String,
                lastName: String,
                avatarUrl: Option[String] = None) {
  lazy val fullName = "%s %s".format(firstName, lastName)
  lazy val emails = UserAlias.listOfEmail(this)
  /**
   * Prepared query for me
   */
  lazy val me = for {
    a <- User
    if (a.id === id)
  } yield a
  /**
   * Delete me
   */
  def delete: Boolean = {
    val v = DB withSession {
      me.delete
    }
    v > 0
  }
  /**
   * Change properties (like a copy) and update Database
   */
  def update(theFirstName: String = firstName, theLastName: String = lastName, theAvatarUrl: Option[String] = avatarUrl): User = {
    val n = copy(lastModifiedAt = Some(DB.now), firstName = theFirstName, lastName = theLastName, avatarUrl = theAvatarUrl)
    DB.withSession {
      me.map { a =>
        (a.lastModifiedAt.? ~ a.firstName ~ a.lastName ~ a.avatarUrl.?)
      }.update(n.lastModifiedAt, n.firstName, n.lastName, n.avatarUrl)
    }
    n
  }
}
object User extends Table[User]("USER") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def firstName = column[String]("FIRST_NAME", O.NotNull)
  def lastName = column[String]("LAST_NAME", O.NotNull)
  def avatarUrl = column[String]("AVATAR_URL", O.Nullable)
  // All columns
  def * = id ~ createdAt ~ lastModifiedAt.? ~ firstName ~ lastName ~ avatarUrl.? <> (User.apply _, User.unapply _)
  /**
   * Add new user
   */
  def addNew(theFirstName: String, theLastName: String, theAvatarUrl: Option[String]): User = {
    val now = DB.now
    val newId = DB withSession {
      def p = createdAt ~ firstName ~ lastName ~ avatarUrl.?
      p returning id insert (now, theFirstName, theLastName, theAvatarUrl)
    }
    User(newId, now, None, theFirstName, theLastName, theAvatarUrl)
  }
  def get(theId: Long): Option[User] = {
    val q = DB withSession {
      for {
        u <- User
        if u.id === theId
      } yield u
    }
    q.firstOption
  }
}

object AlbumOwner extends Table[(Long, Long)]("ALBUM_OWNER") {
  def albumId = column[Long]("ALBUM", O.NotNull)
  def userId = column[Long]("OWNER", O.NotNull)
  // All columns
  def * = albumId ~ userId
  /**
   * Bound album
   */
  def album = foreignKey("ALBUM_OWNER_FK_ALBUM", albumId, Album)(_.id)
  /**
   * Bound user
   */
  def owner = foreignKey("ALBUM_OWNER_FK_OWNER", userId, User)(_.id)
  /**
   * Let user gain album
   */
  def addNew(album: Album, user: User): (Album, User) = {
    DB withSession {
      * insert (album.id, user.id)
    }
    (album, user)
  }
}

object PhotoOwner extends Table[(Long, Long)]("PHOTO_OWNER") {
  def photoId = column[Long]("PHOTO", O.NotNull)
  def userId = column[Long]("OWNER", O.NotNull)
  // All columns
  def * = photoId ~ userId
  /**
   * Bound photo
   */
  def photo = foreignKey("PHOTO_OWNER_FK_PHOTO", photoId, Photo)(_.id)
  /**
   * Bound user
   */
  def owner = foreignKey("PHOTO_OWNER_FK_OWNER", userId, User)(_.id)
  /**
   * Let user gain photo
   */
  def addNew(photo: Photo, user: User): (Photo, User) = {
    DB withSession {
      * insert (photo.id, user.id)
    }
    (photo, user)
  }
}
