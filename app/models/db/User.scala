package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession
import com.amazonaws.util.Md5Utils

case class User(id: Long,
                createdAt: Timestamp,
                lastModifiedAt: Option[Timestamp],
                password: String,
                name: String) {
  lazy val emails = Email.list(this)
  /**
   * Check if given password is correct for this user
   */
  def checkPassword(unhashedPassword: String): Boolean = password == User.hash(unhashedPassword)
  /**
   * Prepared query for me
   */
  val me = for {
    a <- User
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
  def update(password: String = password, name: String = name): User = {
    val n = copy(lastModifiedAt = Some(DB.now), password = password, name = name)
    DB.withSession {
      me.map { a =>
        (a.lastModifiedAt.? ~ a.password ~ a.name)
      }.update(n.lastModifiedAt, n.password, n.name)
    }
    n
  }
}
object User extends Table[User]("USER") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def lastModifiedAt = column[Timestamp]("LAST_MODIFIED_AT", O.Nullable)
  def password = column[String]("PASSWORD_HUSHED", O.NotNull)
  def name = column[String]("NAME", O.NotNull)
  // All columns
  def * = id ~ createdAt ~ lastModifiedAt.? ~ password ~ name <> (User.apply _, User.unapply _)
  /**
   * Add new user
   */
  def addNew(theName: String, unhashedPassword: String): User = {
    val now = DB.now
    val hashed = hash(unhashedPassword)
    val newId = DB withSession {
      def p = createdAt ~ password ~ name
      p returning id insert (now, hashed, theName)
    }
    User(newId, now, None, hashed, theName)
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
  /**
   * Get user by specifying email and password
   */
  def login(theEmail: String, thePassword: String): Option[User] = for {
    u <- Email.getUser(theEmail)
    if u.password == hash(thePassword)
  } yield u
  /**
   * Creating hash of given password
   */
  def hash(unhashedPassword: String): String = {
    val hash = Md5Utils.computeMD5Hash(unhashedPassword.getBytes)
    new String(hash)
  }
}

object Email extends Table[(String, Long)]("EMAIL") {
  def email = column[String]("EMAIL", O.PrimaryKey)
  def userId = column[Long]("USER", O.NotNull)
  // All columns
  def * = email ~ userId
  /**
   * Bound user
   */
  def user = foreignKey("EMAIL_FK_USER", userId, User)(_.id)
  /**
   * Add new email to user
   */
  def addNew(theEmail: String, user: User): (String, User) = {
    val e = DB withSession {
      * returning email insert (theEmail, user.id)
    }
    assert(e == theEmail)
    (e, user)
  }
  /**
   * Get user by specifying account name
   */
  def getUser(theEmail: String): Option[User] = {
    val q = DB withSession {
      for {
        o <- Email
        if o.email === theEmail
        u <- o.user
      } yield u
    }
    q.firstOption
  }
  /**
   * List of email of same user
   */
  def list(user: User): List[String] = {
    val q = DB withSession {
      for {
        e <- Email
        u <- e.user
        if u.id === user.id
      } yield e.email
    }
    q.list
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
    val ui = DB withSession {
      * returning userId insert (album.id, user.id)
    }
    assert(ui == user.id)
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
    val ui = DB withSession {
      * returning userId insert (photo.id, user.id)
    }
    assert(ui == user.id)
    (photo, user)
  }
}
