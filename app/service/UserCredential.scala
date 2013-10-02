package service

import play.api.{ Logger, Application }
import securesocial.core._
import securesocial.core.providers.Token

class UserCredential(application: Application) extends UserServicePlugin(application) {
  import UserCredential._
  import Conversions._

  def find(id: IdentityId): Option[Identity] = {
    Logger.debug(f"Finding user by alias : $id")
    for {
      alias <- models.db.UserAlias.get(id.userId, id.providerId)
    } yield alias
  }

  def findByEmailAndProvider(email: String, providerId: String): Option[Identity] = {
    Logger.debug(f"Finding user by email : $email")
    for {
      alias <- models.db.UserAlias.getByEmail(email)
    } yield alias
  }

  def save(user: Identity): Identity = {
    import models.db._
    withTransaction {
      val u = User.addNew(user.firstName, user.lastName, user.avatarUrl)
      val a1 = UserAlias.addNew(u.id, user.identityId.userId, user.identityId.providerId, 0,
        user.passwordInfo.map(_.password), user.passwordInfo.map(_.hasher))
      val a2 = for {
        email <- user.email
        if (email != a1.name || a1.domain != UserAliasDomain.email)
      } yield UserAlias.addNew(u.id, email, UserAliasDomain.email, 0)
      Logger.debug(f"Saved alias($a1) as $a2")
      a1
    }
  }

  def save(token: Token) {
    val xml = <securesocial-token email={ token.email } isSignUp={ token.isSignUp.toString }/>
    import scala.concurrent.duration._
    val willExpired = (token.expirationTime.getMillis - System.currentTimeMillis).millisecond
    val saved = models.db.VolatileToken.addNew(token.uuid, tokenUses, willExpired, Some(xml.toString))
    Logger.debug("Saved token: %s".format(saved))
  }

  def findToken(token: String): Option[Token] = {
    for (o <- models.db.VolatileToken.get(token, tokenUses)) yield o
  }

  def deleteToken(uuid: String) {
    models.db.VolatileToken.get(uuid, tokenUses).foreach(_.delete)
  }

  def deleteExpiredTokens() {
    models.db.VolatileToken.deleteExpired(Some(tokenUses))
  }
}
object UserCredential {
  /**
   * Mutual conversions: UserAlias <-> Identity
   */
  object Conversions {
    class AliasIdentity(val alias: models.db.UserAlias) extends Identity {
      lazy val identityId = IdentityId(alias.name, alias.domain)
      lazy val firstName = alias.user.firstName
      lazy val lastName = alias.user.lastName
      lazy val fullName = alias.user.fullName
      lazy val email = alias.email
      lazy val avatarUrl = alias.user.avatarUrl
      lazy val authMethod = AuthenticationMethod.UserPassword
      lazy val oAuth1Info: Option[OAuth1Info] = None
      lazy val oAuth2Info: Option[OAuth2Info] = None
      lazy val passwordInfo: Option[PasswordInfo] = for {
        p <- alias.password
        h <- alias.passwordHashing
      } yield PasswordInfo(h, p, None)
    }
    /**
     * Convert: UserAlias -> Identity
     */
    implicit def aliasToIdentity(alias: models.db.UserAlias): Identity = new AliasIdentity(alias)
    /**
     * Convert: Identity -> UserAlias
     */
    implicit def identityToAlias(id: Identity) = id match {
      case ai: AliasIdentity => ai.alias
      case _ => throw new RuntimeException(f"Unsupported identity: $id")
    }
  }
  /**
   * Convert: java.sql.Timestamp -> org.joda.time.DateTime
   */
  implicit def timestampToJoda(dt: _root_.java.sql.Timestamp) = new org.joda.time.DateTime(dt.getTime)
  /**
   * Convert: VolatileToken -> Token
   */
  implicit def volatileToToken(v: models.db.VolatileToken): Token = {
    val xml = v.extra.map(scala.xml.XML loadString _)
    def xmlValue(n: String) = xml.map(_ \ f"@$n").headOption.map(_.toString)
    Token(
      uuid = v.token,
      email = xmlValue("email") getOrElse "",
      creationTime = v.createdAt,
      expirationTime = v.expiration,
      isSignUp = xmlValue("isSignUp").map(_.toBoolean) getOrElse false)
  }
  val tokenUses = models.db.VolatileTokenUses.SecureSocial
}