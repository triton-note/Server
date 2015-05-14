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
    /**
     * 指定された Kind のファイルを生成する。
     */
    def file(f: Kind.type => String)(cognitoId: String, sessionId: String, left: String*) =
      Storage.file(("user" :: cognitoId :: "photo" :: sessionId :: f(Kind) :: left.toList): _*)
    /**
     * オリジナルの画像ファイルから Reduced の画像ファイルのリストを作り出す。
     */
    def reduced(original: Storage.S3File): List[Storage.S3File] = settings.image.size.toList.map {
      case (relation, _) => original.paths match {
        case "user" :: cognitoId :: "photo" :: sessionId :: Kind.ORIGINAL :: name :: Nil =>
          file(_.REDUCED)(cognitoId, sessionId, relation, name)
        case _ => throw new IllegalArgumentException(f"Unexpected file path: ${original.paths}")
      }
    }
  }
  implicit val json = Json.format[Photo]

  /**
   * オリジナルの画像をリサイズして Reduced 画像を作成し、セットとした Photo インスタンスを生成する。
   * この時、オリジナル以外の画像の向きは正立させられる。
   */
  def of(user: User, sessionId: String, filename: String): Option[Photo] = allCatch.opt {
    val file = Image.file(_.ORIGINAL)(user.cognitoId, sessionId, filename)
    val rotated = {
      val image = scrimage.Image(file.read)
      allCatch.opt {
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
    }
    val images = settings.image.size.map {
      case (relation, maxSize) => relation -> {
        val file = Image.file(_.REDUCED)(user.cognitoId, sessionId, relation, filename)
        val (width, height) = (rotated.width, rotated.height)
        val (w, h) = if (width > height) (maxSize, height * maxSize / width) else (width * maxSize / height, maxSize)
        Logger debug f"Resizing image for ${relation}: (${width} x ${height}) -> (${w} x ${h})"
        val scaled = rotated.scaleTo(w, h)
        scaled.writer(scrimage.Format.JPEG).write(file.newWriter("image/jpeg"))
        Photo.Image(file)
      }
    }
    Photo(Photo.Image(file), images)
  }
}
