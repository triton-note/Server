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
  def me = for {
    o <- VolatileToken
    if (o.token === token)
    if (o.uses === uses)
  } yield o
  /**
   * Delete me
   */
  def delete: Boolean = withSession {
    me.delete > 0
  }
  /**
   * Reload form DB
   */
  def refresh: Option[VolatileToken] = VolatileToken.get(token, uses)
  def update(theExtra: Option[String] = extra, theExpiration: Timestamp = expiration) = {
    val n = copy(extra = theExtra, expiration = theExpiration)
    withSession {
      me.map { a =>
        (a.expiration ~ a.extra.?)
      }.update(theExpiration, theExtra)
    }
    n
  }
  def setExtra(xml: scala.xml.Elem) = update(theExtra = Some(xml.toString))
  def removeExtra = update(theExtra = None)
  def changeExpiration(theNext: Timestamp) = update(theExpiration = theNext)
}
object VolatileToken extends Table[VolatileToken]("VOLATILE_TOKEN") {
  def token = column[String]("TOKEN", O.NotNull)
  def uses = column[String]("USES", O.NotNull)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def expiration = column[Timestamp]("EXPIRATION", O.NotNull)
  def extra = column[String]("EXTRA", O.Nullable, O.DBType("TEXT"))
  // Define primary key
  def pk = primaryKey("VOLATILE_TOKEN_PK", (token, uses))
  // All columns
  def * = token ~ uses ~ createdAt ~ expiration ~ extra.? <> (VolatileToken.apply _, VolatileToken.unapply _)
  /**
   * Add new token
   */
  def addNew(theToken: String, theUses: String, willExpired: scala.concurrent.duration.FiniteDuration, extra: Option[String] = None): VolatileToken = {
    val o = VolatileToken(theToken, theUses, currentTimestamp, new Timestamp(currentTimestamp.getTime + willExpired.toMillis), extra)
    withSession {
      * insert o
    }
    o
  }
  /**
   * Create new token and save to database
   */
  def createNew(willExpired: scala.concurrent.duration.FiniteDuration, extra: Option[String] = None): VolatileToken = {
    val token = play.api.libs.Codecs.sha1(System.currentTimeMillis.toString)
    addNew(token, VolatileTokenUses.Application, willExpired, extra)
  }
  /**
   * Obtain specified token
   */
  def get(theToken: String, theUses: String = VolatileTokenUses.Application): Option[VolatileToken] = withSession {
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
  def deleteExpired(theUses: Option[String]): Int = withSession {
    val q = for {
      o <- VolatileToken
      if (o.expiration > currentTimestamp)
    } yield o
    q.delete
  }
}
object VolatileTokenUses {
  val Application = "TritonNote"
  val SecureSocial = "SecureSocial"
}
