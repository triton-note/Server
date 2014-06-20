package models

import java.io.{BufferedInputStream, FileInputStream}
import java.util.Date

import scala.annotation.implicitNotFound
import scala.concurrent.duration._
import scala.util.control.Exception.allCatch

import play.api.Logger
import play.api.libs.json._

import com.amazonaws.services.s3.model.ObjectMetadata

object Storage {
  lazy val s3 = service.AWS.S3.client
  lazy val bucketName = service.AWS.S3.bucketName
  def retry[T](count: Int)(proc: => T): Option[T] = {
    var down = count
    while (down > 0) {
      try {
        return Some(proc)
      } catch {
        case ex: Exception => {
          Thread.sleep(1000)
          down -= 1
        }
      }
    }
    None
  }
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
      val obj = s3.getObject(bucketName, path)
      obj.getObjectMetadata.getContentLength
    }
    def exists: Boolean = {
      s3.getObjectMetadata(bucketName, path) != null
    }
    def write(src: java.io.File, retryCount: Int = Settings.Strage.retryLimit): Boolean = {
      import java.io._
      slurp(new BufferedInputStream(new FileInputStream(src)), retryCount)
    }
    def slurp(source: java.io.InputStream, retryCount: Int = Settings.Strage.retryLimit): Boolean = {
      Logger.debug(f"Storing for S3:${bucketName}:${path}")
      allCatch opt s3.putObject(bucketName, path, source, new ObjectMetadata()) match {
        case None    => if (retryCount > 0) slurp(source, retryCount - 1) else false
        case Some(_) => true
      }
    }
    def delete: Boolean = {
      (allCatch opt s3.deleteObject(bucketName, path)).isDefined
    }
    def read: java.io.InputStream = {
      val obj = s3.getObject(bucketName, path)
      obj.getObjectContent
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
}
