package models

import java.io.{ BufferedInputStream, BufferedOutputStream, IOException, OutputStream, PipedInputStream, PipedOutputStream }
import java.util.Date

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._

import play.api.Logger
import play.api.libs.json.{ Format, Json, Writes, __ }

import org.fathens.play.util.Exception.allCatch

import com.amazonaws.services.s3.model.ObjectMetadata

object Storage {
  lazy val s3 = service.AWS.S3.client
  lazy val bucketName = service.AWS.S3.bucketName
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
}
