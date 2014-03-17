package controllers

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.control.Exception._
import ExecutionContext.Implicits.global
import play.api.Logger
import play.api.data._
import play.api.mvc._
import models._
import service._
import service.UserCredential.Conversions._

object InitPhoto extends Controller with securesocial.core.SecureSocial {
  val sessionFacebook = new SessionValue("TritonNote-facebook_accesskey", Settings.Session.timeoutFacebook)
  val sessionUploading = new SessionValue("TritonNote-uploading", Settings.Session.timeoutUpload)
  /**
   * Register authorized user to session
   */
  private def completeAuth(ses: (String, String)*)(implicit user: db.User) = {
    securesocial.core.Authenticator.create(user) match {
      case Left(error) => throw error
      case Right(authenticator) => {
        val cookies = List(authenticator.toCookie)
        val extra = <header>
                      {
                        for {
                          c <- cookies
                        } yield <cookie name={ c.name } value={ c.value } maxAge={ c.maxAge.map(_.toString) orNull }/>
                      }{
                        for {
                          (name, value) <- ses
                        } yield <session name={ name } value={ value }/>
                      }
                    </header>
        Logger debug f"Saving cookies and session as XML: $extra"
        val vt = db.VolatileTokens.createNew(5 minutes, Some(extra.toString))
        Ok(vt.id).withCookies(cookies: _*).withSession(ses: _*)
      }
    }
  }
  /**
   * Login as Facebook user
   */
  def createTokenByFacebook = Action.async(parse.text) { implicit request =>
    val accesskey = request.body
    for {
      u <- Facebook.User(accesskey)
    } yield {
      Logger debug f"Authorized user from facebook: $u"
      u map { implicit user =>
        completeAuth(
          sessionFacebook(accesskey),
          sessionUploading()
        )
      } getOrElse Results.Unauthorized
    }
  }
  /**
   * ファイル情報の XML を保存して、
   * 推測された情報を返す。
   */
  def saveInfo = SecuredAction(parse.xml) { implicit request =>
    val ok = for {
      vt <- sessionUploading(request)
      xml <- request.body.headOption
      info <- PreInfo(xml)
    } yield {
      val infered = InferencePreInfo.infer(info).toXML
      vt.refresh.map(_ setExtra infered)
      Ok(infered)
    }
    ok getOrElse Results.BadRequest
  }
  /**
   * Set initializing info by user
   */
  def submit = SecuredAction(parse.xml) { implicit request =>
    implicit val user: db.User = request.user
    Logger.info(f"Uploaded form body: ${request.body}")
    val ok = for {
      vt <- sessionUploading(request)
      xml <- request.body.headOption
      info <- PreInfo(xml)
      if info.submission.isDefined
    } yield {
      vt.setExtra(info.toXML)
      commit(info)
      Ok("Submitted")
    }
    ok getOrElse Results.BadRequest
  }
  def cancel = SecuredAction(parse.text) { implicit request =>
    val ok = for {
      vt <- sessionUploading(request)
      xml <- vt.extra
      info <- PreInfo(xml)
      if info.basic.filepath == request.body
    } yield {
      Logger.debug(f"Canceling ${info.basic.filepath}")
      if (vt.delete) Ok("Canceled") else NoContent
    }
    ok getOrElse Results.BadRequest
  }
  /**
   * Uploaded photo data
   */
  def upload = SecuredAction(parse.raw) { implicit request =>
    implicit val user: db.User = request.user
    val ok = for {
      vt <- sessionUploading(request)
      xml <- vt.extra
      info <- PreInfo(xml)
    } yield {
      val tmp = request.body.asFile
      Logger.trace(f"Uploaded $tmp for $vt")
      info.upload(tmp)
      commit(info)
      Ok("Uploaded")
    }
    ok getOrElse Results.BadRequest
  }
  def commit(info: PreInfo)(implicit user: db.User, request: Request[_]): Option[Future[Int]] = {
    for {
      fishes <- info.commit(user)
      fb <- sessionFacebook(request)
      key <- fb.extra
    } yield {
      implicit val ak = Facebook.AccessKey(key)
      PublishPhoto.publish(fishes).map(_.size)
    }
  }
}
