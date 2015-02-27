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
  mainview: Photo.Image,
  thumbnail: Photo.Image) {
  def delete = {
    List(original, mainview, thumbnail).par.map(_.file.delete)
  }
}
object Photo {
  case class Image(file: Storage.S3File)
  object Image {
    implicit val json = Format[Image](
      (__ \ "path").read[String].map(Storage file _).map(Image.apply),
      Writes { image =>
        Json.obj(
          "path" -> image.file.path,
          "volatileUrl" -> image.file.generateURL(settings.image.urlTimeout).toString
        )
      }
    )
    object Kind extends Enumeration {
      val ORIGINAL = Value("original")
      val REDUCED = Value("reduced")
    }
  }
  implicit val json = Json.format[Photo]

  /**
   * オリジナルの画像をリサイズして Mainview と Thumbnail 専用の画像を作成する。
   */
  def of(file: Storage.S3File): Option[Photo] = {
    def resize(image: scrimage.Image, max: Int, relation: String) = {
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
        case name :: parent :: left => List("photo", Photo.Image.Kind.REDUCED, parent, relation).mkString("/")
        case _                      => throw new IllegalArgumentException(f"Unexpected file path: ${file.paths}")
      }
      val dstFile = Storage.file(path)
      scaled.writer(scrimage.Format.JPEG).write(dstFile.newWriter)
      Photo.Image(dstFile)
    }
    for {
      i <- allCatch.opt { Option(scrimage.Image(file.read)) }.flatten
      m <- allCatch opt resize(i, settings.image.size.mainview, "mainview")
      t <- allCatch opt resize(i, settings.image.size.thumbnail, "thumbnail")
    } yield Photo(Photo.Image(file), m, t)
  }
}
