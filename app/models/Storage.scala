package models

import scala.concurrent.duration._
import play.{Logger => Log}
import com.amazonaws.auth.BasicAWSCredentials
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
    Log.trace(f"Creating S3File: $paths")
    new S3File(paths.toList)
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
    def write(src: java.io.File): Boolean = {
      import java.io._
      write(new BufferedInputStream(new FileInputStream(src)))
    }
    def write(source: java.io.InputStream): Boolean = {
      Log.debug(f"Storing for S3:${bucketName}:${path}")
      s3.putObject(bucketName, path, source, new ObjectMetadata())
      true
    }
    def delete: Boolean = {
      s3.deleteObject(bucketName, path)
      true
    }
    def read: java.io.InputStream = {
      val obj = s3.getObject(bucketName, path)
      obj.getObjectContent
    }
    def move(dstPaths: String*): S3File = {
      val dstFile = new S3File(dstPaths.toList)
      dstFile write read
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
