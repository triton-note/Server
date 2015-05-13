package models

import play.api.Logger
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper

import org.fathens.play.util.Exception.allCatch

import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.exif.ExifIFD0Directory
import com.sksamuel.scrimage

import service.Storage

case class Photo(
  original: Photo.Image,
  reduced: Map[String, Photo.Image]) {
  def delete = {
    (original :: reduced.values.toList).par.map(_.file.delete)
  }
}
object Photo {
  case class Image(file: Storage.S3File)
  object Image {
    implicit val json = Format[Image](
      (__ \ "path").read[String].map(Storage file _).map(Image.apply),
      Writes { image => Json.obj("path" -> image.file.path) }
    )
    object Kind {
      val ORIGINAL = "original"
      val REDUCED = "reduced"
    }
    def originalFolder(userId: String, sessionId: String) =
      List("user", userId, "photo", sessionId, Kind.ORIGINAL).mkString("/")
  }
  implicit val json = Json.format[Photo]

  case class ReducedImage(original: Storage.S3File, relation: String, maxSize: Int) {
    val file = original.paths.reverse match {
      case name :: _ :: parent => Storage.file((name :: relation :: Image.Kind.REDUCED :: parent).reverse: _*)
      case _                   => throw new IllegalArgumentException(f"Unexpected file path: ${original.paths}")
    }
    def writeImage(rotated: scrimage.Image) = {
      val (width, height) = (rotated.width, rotated.height)
      val (w, h) = if (width > height) (maxSize, height * maxSize / width) else (width * maxSize / height, maxSize)
      Logger debug f"Resizing image for ${relation}: (${width} x ${height}) -> (${w} x ${h})"
      val scaled = rotated.scaleTo(w, h)
      scaled.writer(scrimage.Format.JPEG).write(file.newWriter("image/jpeg"))
      Photo.Image(file)
    }
  }
  object ReducedImage {
    def apply(original: Storage.S3File): Map[String, ReducedImage] = settings.image.size.map {
      case (relation, max) => relation -> ReducedImage(original, relation, max)
    }
  }

  /**
   * オリジナルの画像をリサイズして Reduced 専用の画像を作成する。
   * この時、画像の向きは正立させられる。
   */
  def of(file: Storage.S3File): Option[Photo] = allCatch.opt {
    val image = scrimage.Image(file.read)
    val rotated = allCatch.opt {
      val metadata = ImageMetadataReader.readMetadata(file.read)
      val dic = metadata.getDirectory(classOf[ExifIFD0Directory])
      if (dic == null) image
      else dic.getInt(ExifIFD0Directory.TAG_ORIENTATION) match {
        case 3 => image.rotateLeft.rotateLeft
        case 6 => image.rotateLeft
        case 8 => image.rotateRight
        case _ => image
      }
    } getOrElse image
    val images = ReducedImage(file).map {
      case (relation, file) => relation -> file.writeImage(rotated)
    }
    Photo(Photo.Image(file), images)
  }
}
