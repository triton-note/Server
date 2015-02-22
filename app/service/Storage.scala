package service

import java.io.{ BufferedInputStream, BufferedOutputStream, IOException, OutputStream, PipedInputStream, PipedOutputStream }
import java.nio.charset.Charset
import java.util.Date

import scala.concurrent.Future
import scala.concurrent.duration._

import play.api.Logger
import play.api.libs.{ Codecs, Crypto }
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax.{ functionalCanBuildApplicative, toFunctionalBuilderOps, unlift }
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

import org.apache.commons.codec.binary.Base64
import org.fathens.play.util.Exception.allCatch

import com.amazonaws.services.s3.model.ObjectMetadata

object Storage {
  lazy val s3 = AWS.S3.client
  lazy val bucketName = AWS.S3.bucketName
  def file(paths: String*) = {
    val list = paths.map(_ split "/").flatten.toList
    Logger.trace(f"Creating S3File: $list")
    new S3File(list)
  }
  object S3File {
    implicit val photostorageFormat = Format[Storage.S3File](
      __.read[String].map(Storage file _),
      Writes(Json toJson _.path)
    )
  }
  class S3File(val paths: List[String]) {
    lazy val path = paths.mkString("/")
    lazy val name = paths.last
    override def toString = f"S3:$path"
    def length: Long = {
      val meta = s3.getObjectMetadata(bucketName, path)
      meta.getContentLength
    }
    def exists: Boolean = {
      s3.getObjectMetadata(bucketName, path) != null
    }
    def newWriter: OutputStream = {
      val ins = new PipedInputStream
      val out = new PipedOutputStream(ins)
      Future {
        slurp(new BufferedInputStream(ins))
      }
      new BufferedOutputStream(out)
    }
    def slurp(source: java.io.InputStream) {
      Logger.debug(f"Storing for S3:${bucketName}:${path}")
      try {
        s3.putObject(bucketName, path, source, new ObjectMetadata())
      } catch {
        case ex: Exception => throw new IOException(f"Failed to load to ${this}", ex)
      }
    }
    def delete: Boolean = {
      (allCatch opt s3.deleteObject(bucketName, path)).isDefined
    }
    def read: java.io.InputStream = {
      val obj = s3.getObject(bucketName, path)
      val ins = new java.io.BufferedInputStream(obj.getObjectContent)
      // available が 0 になっているのでバッファに少しは読み込ませる
      ins.mark(1)
      ins.skip(1)
      ins.reset
      ins
    }
    def move(dstPaths: String*): S3File = {
      val dstFile = new S3File(dstPaths.toList)
      dstFile slurp read
      delete
      dstFile
    }
    def generateURL(expire: FiniteDuration): java.net.URL = {
      import java.util.Date
      val date = new Date(new Date().getTime + expire.toMillis)
      s3.generatePresignedUrl(bucketName, path, date)
    }
  }

  /**
   * Client からアップロードさせるための情報を作成する
   */
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
}
