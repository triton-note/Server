import play.api.Logger
import play.api.libs.json._
import play.api.mvc.Results.BadRequest

import org.fathens.play.util.Exception.allCatch

import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.exif.ExifIFD0Directory
import com.sksamuel.scrimage.{ Format, Image }

import models.{ Report, User, VolatileToken }
import service.{ Settings, Storage }

package object controllers {
  val TicketExpired = BadRequest("Ticket Expired")
  val SessionExpired = BadRequest("Session Expired")

  object TicketValue {
    implicit val ticketFormat = Json.format[TicketValue]
  }
  case class TicketValue(
    userId: String,
    way: String,
    token: String) {
    override def toString = Json.toJson(this).toString
  }
  /**
   * 文字列を token として扱い、ユーザIDが含まれている事を確認する拡張
   */
  implicit class TokenOfUser(token: String) {
    def asTokenOfUser[T <: { val userId: String }](implicit reads: Reads[T]) = for {
      vt <- VolatileToken get token
      value <- vt.data.asOpt[T]
      user <- User get value.userId
    } yield (vt, value, user)
  }

  /**
   * オリジナルの画像をリサイズして Mainview と Thumbnail 専用の画像を作成する。
   */
  def asPhoto(file: Storage.S3File): Option[Report.Photo] = {
    def resize(image: Image, max: Int, relation: String) = {
      val rotated = allCatch.opt {
        val metadata = ImageMetadataReader.readMetadata(file.read)
        val dic = metadata.getDirectory(classOf[ExifIFD0Directory])
        dic.getInt(ExifIFD0Directory.TAG_ORIENTATION) match {
          case 3 => image.rotateLeft.rotateLeft
          case 6 => image.rotateLeft
          case 8 => image.rotateRight
          case _ => image
        }
      } getOrElse image
      val (width, height) = (rotated.width, rotated.height)
      val (w, h) = if (width > height) (max, height * max / width) else (width * max / height, max)
      Logger debug f"Resizing image for ${relation}: (${width} x ${height}) -> (${w} x ${h})"
      val scaled = rotated.scaleTo(w, h)
      val path = file.paths.reverse match {
        case name :: parent :: left => List("photo", Report.Photo.Kind.REDUCED, parent, relation).mkString("/")
        case _                      => throw new IllegalArgumentException(f"Unexpected file path: ${file.paths}")
      }
      val dstFile = Storage.file(path)
      scaled.writer(Format.JPEG).write(dstFile.newWriter)
      dstFile
    }
    for {
      i <- allCatch.opt { Option(Image(file.read)) }.flatten
      m <- allCatch opt resize(i, Settings.Image.sizeMainview, "mainview")
      t <- allCatch opt resize(i, Settings.Image.sizeThumbnail, "thumbnail")
    } yield Report.Photo(file, m, t)
  }
}
