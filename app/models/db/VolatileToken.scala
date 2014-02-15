package models.db

import java.sql.Timestamp
import simple._
import scalaz._
import Scalaz._

case class VolatileToken(token: String,
                         uses: String,
                         createdAt: Timestamp,
                         expiration: Timestamp,
                         extra: Option[String]) {
  /**
   * Query for me
   */
  private def me = VolatileTokens.filter(o => (o.token is token) && (o.uses is uses))
  /**
   * Delete me
   */
  def delete: Boolean = DB withSession { implicit session =>
    me.delete > 0
  }
  /**
   * Reload form DB
   */
  def refresh: Option[VolatileToken] = VolatileTokens.get(token, uses)
  /**
   * Change properties (like a copy) and update Database
   */
  def update(extra: Option[String] = extra, expiration: Timestamp = expiration): Option[VolatileToken] = {
    DB withSession { implicit session =>
      me.map { a =>
        (a.expiration, a.extra.?)
      }.update((expiration, extra)) == 1
    } option copy(extra = extra, expiration = expiration)
  }
  def setExtra(xml: scala.xml.Elem) = update(extra = Some(xml.toString))
  def removeExtra = update(extra = None)
  def changeExpiration(theNext: Timestamp) = update(expiration = theNext)
}
class VolatileTokens(tag: Tag) extends Table[VolatileToken](tag, "VOLATILE_TOKEN") {
  def token = column[String]("TOKEN", O.NotNull)
  def uses = column[String]("USES", O.NotNull)
  def createdAt = column[Timestamp]("CREATED_AT", O.NotNull)
  def expiration = column[Timestamp]("EXPIRATION", O.NotNull)
  def extra = column[String]("EXTRA", O.Nullable, O.DBType("TEXT"))
  // Define primary key
  def pk = primaryKey("VOLATILE_TOKEN_PK", (token, uses))
  // All columns
  def * = (token, uses, createdAt, expiration, extra.?) <> (VolatileToken.tupled, VolatileToken.unapply)
}
object VolatileTokens extends TableQuery(new VolatileTokens(_)) {
  /**
   * Add new token
   */
  def addNew(token: String, uses: String, willExpired: scala.concurrent.duration.FiniteDuration, extra: Option[String] = None): Option[VolatileToken] = {
    val o = VolatileToken(token, uses, currentTimestamp, new Timestamp(currentTimestamp.getTime + willExpired.toMillis), extra)
    DB withSession { implicit session =>
      (this += o) == 1
    } option o
  }
  /**
   * Create new token and save to database
   */
  def createNew(willExpired: scala.concurrent.duration.FiniteDuration, extra: Option[String] = None): Option[VolatileToken] = {
    val token = play.api.libs.Codecs.sha1(System.currentTimeMillis.toString)
    addNew(token, VolatileTokenUses.Application, willExpired, extra)
  }
  /**
   * Obtain specified token
   */
  def get(token: String, uses: String = VolatileTokenUses.Application): Option[VolatileToken] = DB withSession { implicit session =>
    filter { o =>
      (o.token is token) && (o.uses is uses)
    }.firstOption
  }
  /**
   * Delete expired tokens
   * @return number of deleted
   */
  def deleteExpired(theUses: Option[String]): Int = DB withSession { implicit session =>
    filter(_.expiration > currentTimestamp).delete
  }
}
object VolatileTokenUses {
  val Application = "TritonNote"
  val SecureSocial = "SecureSocial"
}
