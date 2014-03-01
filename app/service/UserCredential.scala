package service

import models.db._
import play.api.{ Logger, Application }
import securesocial.core._
import securesocial.core.providers.Token

class UserCredential(application: Application) extends UserServicePlugin(application) {
  import UserCredential._
  import Conversions._

  def find(id: IdentityId): Option[Identity] = {
    Logger.debug(f"Finding user by alias : $id")
    for {
      alias <- Users.get(id.userId)
    } yield alias
  }

  def findByEmailAndProvider(email: String, providerId: String): Option[Identity] = {
    Logger.debug(f"Finding user by email : $email")
    for {
      alias <- Users.get(email)
    } yield alias
  }

  def save(user: Identity): Option[Identity] = {
    for {
      email <- user.email
      u <- Users.addNew(email, user.passwordInfo.map(_.password), user.firstName, user.lastName, user.avatarUrl)
    } yield {
      Logger.debug(f"Saved as $u")
      u
    }
  }

  def save(token: Token) {
    val xml = <securesocial-token email={ token.email } isSignUp={ token.isSignUp.toString }/>
    import scala.concurrent.duration._
    val willExpired = (token.expirationTime.getMillis - System.currentTimeMillis).millisecond
    val saved = VolatileTokens.addNew(token.uuid, willExpired, Some(xml.toString))
    Logger.debug(f"Saved token: $saved")
  }

  def findToken(token: String): Option[Token] = {
    for (o <- VolatileTokens.get(token)) yield o
  }

  def deleteToken(uuid: String) {
    VolatileTokens.get(uuid).foreach(_.delete)
  }

  def deleteExpiredTokens() {
    VolatileTokens.deleteExpired
  }
}
object UserCredential {
  val domain = ""
  /**
   * Mutual conversions: UserAlias <-> Identity
   */
  object Conversions {
    class AliasIdentity(val alias: User) extends Identity {
      lazy val identityId = IdentityId(alias.id, domain)
      lazy val firstName = alias.firstName
      lazy val lastName = alias.lastName
      lazy val fullName = alias.fullName
      lazy val email = alias.id
      lazy val avatarUrl = alias.avatarUrl
      lazy val authMethod = AuthenticationMethod.UserPassword
      lazy val oAuth1Info: Option[OAuth1Info] = None
      lazy val oAuth2Info: Option[OAuth2Info] = None
      lazy val passwordInfo: Option[PasswordInfo] = for {
        p <- alias.password
        h = Users.hashingWay
      } yield PasswordInfo(h, p, None)
    }
    /**
     * Convert: UserAlias -> Identity
     */
    implicit def aliasToIdentity(alias: User): Identity = new AliasIdentity(alias)
    /**
     * Convert: Identity -> UserAlias
     */
    implicit def identityToAlias(id: Identity) = id match {
      case ai: AliasIdentity => ai.alias
      case _                 => throw new RuntimeException(f"Unsupported identity: $id")
    }
  }
  /**
   * Convert: java.util.Date -> org.joda.time.DateTime
   */
  implicit def dateToJoda(dt: _root_.java.util.Date) = new org.joda.time.DateTime(dt.getTime)
  /**
   * Convert: VolatileToken -> Token
   */
  implicit def volatileToToken(v: models.db.VolatileToken): Token = {
    val xml = v.extra.map(scala.xml.XML loadString _)
    def xmlValue(n: String) = xml.map(_ \ f"@$n").headOption.map(_.toString)
    Token(
      uuid = v.id,
      email = xmlValue("email") getOrElse "",
      creationTime = v.createdAt,
      expirationTime = v.expiration,
      isSignUp = xmlValue("isSignUp").map(_.toBoolean) getOrElse false)
  }
}