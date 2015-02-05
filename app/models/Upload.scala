package models

import java.nio.charset.Charset

import scala.concurrent.duration._

import play.api.libs.{ Codecs, Crypto }
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

import org.apache.commons.codec.binary.Base64

import service.AWS

object Upload {
  case class Start(
    url: String,
    params: Params)
  object Start {
    implicit val startFormat = Json.format[Start]
  }

  case class Params(
    key: String,
    accessKey: String,
    acl: String,
    policy: String,
    signature: String,
    contentType: String)
  object Params {
    implicit val paramsFormat: Format[Params] = (
      (__ \ "key").format[String] and
      (__ \ "AWSAccessKeyId").format[String] and
      (__ \ "acl").format[String] and
      (__ \ "policy").format[String] and
      (__ \ "signature").format[String] and
      (__ \ "Content-Type").format[String]
    )(Params.apply, unlift(Params.unapply))
  }

  def expiration(dur: FiniteDuration) = {
    val now = new java.util.Date
    val limit = new java.util.Date(now.getTime + dur.toMillis)
    com.amazonaws.util.DateUtils.formatISO8601Date(limit)
  }
  def start(folderPath: String) = {
    val contentType = "image/jpeg"
    val (policy, signature) = {
      val charset = Charset.forName("UTF-8")
      val src = Json.obj(
        "expiration" -> expiration(Settings.Image.uploadExpiration),
        "conditions" -> Json.arr(
          Json.obj("bucket" -> AWS.S3.bucketName),
          Json.arr("starts-with", "$key", folderPath),
          Json.obj("acl" -> AWS.S3.ClientSide.acl),
          Json.obj("Content-Type" -> contentType),
          Json.arr("content-length-range", 1, Settings.Image.uploadMaxSize)
        )
      )
      val base64 = Base64 encodeBase64String src.toString.getBytes(charset)
      val sig = {
        val hex = Crypto.sign(base64, AWS.S3.ClientSide.seacretKey.getBytes(charset))
        Base64 encodeBase64String Codecs.hexStringToByte(hex)
      }
      (base64, sig)
    }
    Start(
      AWS.S3.ClientSide.targetUrl,
      Params(
        folderPath + "/${filename}",
        AWS.S3.ClientSide.accessKey,
        AWS.S3.ClientSide.acl,
        policy,
        signature,
        contentType
      )
    )
  }
}