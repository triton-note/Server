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
    val path = paths.mkString("/")
    Log.trace("Creating S3File: " + path)
    new S3File(path)
  }
  class S3File(val path: String) {
    /**
     * ファイルが存在しているかどうかを返す。
     */
    def exists: Boolean = {
      s3.getObjectMetadata(bucketName, path) != null
    }
    /**
     * ファイルに書き込む。
     * 存在していた場合は上書きになる。
     */
    def write(source: InputStream): Boolean = {
      Log.debug("Storing for S3:%s:%s".format(bucketName, path))
      s3.putObject(bucketName, path, source, new ObjectMetadata())
      true
    }
    /**
     * ファイルを削除する。
     * 存在していなくても true を返す。
     */
    def delete: Boolean = {
      s3.deleteObject(bucketName, path)
      true
    }
    /**
     * ファイルを読み込む。
     */
    def read: InputStream = {
      val obj = s3.getObject(bucketName, path)
      obj.getObjectContent
    }
    def generateURL(expireSeconds: Long): java.net.URL = {
      import java.util.Date
      val date = new Date(new Date().getTime + expireSeconds * 1000)
      s3.generatePresignedUrl(bucketName, path, date)
    }
  }
}
