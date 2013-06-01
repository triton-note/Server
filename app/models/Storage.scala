package models

import play.{Logger => Log}
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3Client
import java.io.InputStream
import com.amazonaws.services.s3.model.ObjectMetadata
import com.amazonaws.services.s3.model.DeleteObjectRequest

object Storage {
  val bucketName = System.getenv("S3_BUCKET_NAME")
  val s3 = {
    val id = System.getenv("AWS_ACCESS_KEY_ID")
    val key = System.getenv("AWS_SECRET_ACCESS_KEY")
    val credential = new BasicAWSCredentials(id, key)
    new AmazonS3Client(credential)
  }
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
    Log.trace("Creating S3File: " + paths)
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
      val ins = new BufferedInputStream(new FileInputStream(src))
      write(ins)
    }
    def write(source: InputStream): Boolean = {
      Log.debug("Storing for S3:%s:%s".format(bucketName, path))
      s3.putObject(bucketName, path, source, new ObjectMetadata())
      true
    }
    def delete: Boolean = {
      s3.deleteObject(bucketName, path)
      true
    }
    def read: InputStream = {
      val obj = s3.getObject(bucketName, path)
      obj.getObjectContent
    }
    def move(dstPaths: String*): S3File = {
      val dstFile = new S3File(dstPaths.toList)
      dstFile write read
      delete
      dstFile
    }
    def generateURL(expire: scala.concurrent.duration.FiniteDuration): java.net.URL = {
      import java.util.Date
      val date = new Date(new Date().getTime + expire.toMillis)
      s3.generatePresignedUrl(bucketName, path, date)
    }
  }
}
