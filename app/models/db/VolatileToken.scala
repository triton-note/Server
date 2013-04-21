package models.db

import java.sql.Timestamp
import DB.simple._
import Database.threadLocalSession

case class VolatileToken(token: String,
                         uses: String,
                         createdAt: Timestamp,
                         expiration: Timestamp,
                         extra: Option[String]) {
  /**
   * Prepared query for me
   */
  lazy val me = for {
    o <- VolatileToken
    if (o.token === token)
    if (o.uses === uses)
  } yield o
  /**
   * Delete me
   */
  def delete: Boolean = {
    val v = DB withSession {
      me.delete
    }
    v > 0
  }
}
object VolatileToken extends Table[VolatileToken]("VOLATILE_TOKEN") {
  def token = column[String]("TOKEN", O.NotNull)
  def uses = column[String]("USES", O.NotNull)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def expiration = column[Timestamp]("EXPIRATION", O.NotNull)
  def extra = column[String]("EXTRA", O.Nullable)
  // Define primary key
  def pk = primaryKey("VOLATILE_TOKEN_PK", (token, uses))
  // All columns
  def * = token ~ uses ~ createdAt ~ expiration ~ extra.? <> (VolatileToken.apply _, VolatileToken.unapply _)
  /**
   * Add new token
   */
  def addNew(theToken: String, theUses: String, willExpired: scala.concurrent.duration.FiniteDuration, extra: Option[String] = None): VolatileToken = {
    val o = VolatileToken(theToken, theUses, DB.now, new Timestamp(DB.now.getTime + willExpired.toMillis), extra)
    DB withSession {
      * insert o
    }
    o
  }
  /**
   * Obtain specified token
   */
  def get(theToken: String, theUses: String): Option[VolatileToken] = DB withSession {
    val q = for {
      o <- VolatileToken
      if (o.token === theToken)
      if (o.uses === theUses)
    } yield o
    q.firstOption
  }
  /**
   * Delete expired tokens
   * @return number of deleted
   */
  def deleteExpired(theUses: Option[String]): Int = DB withSession {
    val q = for {
      o <- VolatileToken
      if (o.expiration > DB.now)
    } yield o
    q.delete
  }
}
object VolatileTokenUsers {
  val SecureSocial = "SecureSocial"
}
