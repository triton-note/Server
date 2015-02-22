import play.api.Logger
import play.api.libs.json._
import play.api.mvc.Results.BadRequest

import org.fathens.play.util.Exception.allCatch

import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.exif.ExifIFD0Directory
import com.sksamuel.scrimage.{ Format, Image => ScrImage }

import models.{ Report, Storage }
import models.db.{ FishSize, Image, ImageRelation, Photo, User, VolatileToken }
import service.Settings

package object controllers {
  object TicketValue {
    implicit val ticketFormat = Json.format[TicketValue]
  }
  case class TicketValue(
    userId: String,
    way: String,
    token: String) {
    override def toString = Json.toJson(this).toString
  }
  case class Photos(
    original: Image,
    mainview: Image,
    thumbnail: Image) {
    def asURL = Report.Photo(
      original.url(Settings.Image.urlExpiration).toString,
      mainview.url(Settings.Image.urlExpiration).toString,
      thumbnail.url(Settings.Image.urlExpiration).toString)
  }
  /**
   * Token に紐付けして保存してある情報を JSON と読み書きする
   */
  implicit class VolatileJson(vt: VolatileToken) {
    def json[T](implicit reads: Reads[T]): Option[T] = for {
      extra <- vt.extra
      json <- allCatch opt Json.parse(extra)
      value <- json.asOpt[T]
    } yield value
    def json[T](value: T)(implicit writes: Writes[T]): Option[VolatileToken] = for {
      json <- allCatch opt Json.toJson(value)
      next <- vt setExtra json.toString
    } yield next
  }
  /**
   * 文字列を token として扱い、ユーザIDが含まれている事を確認する拡張
   */
  implicit class TokenOfUser(token: String) {
    def asTokenOfUser[T <: { val userId: String }](implicit reads: Reads[T]) = for {
      vt <- VolatileToken get token
      value <- vt.json[T]
      user <- User get value.userId
    } yield (vt, value, user)
  }
  implicit class FishDB(fish: Report.Fishes) {
    def same(o: FishSize): Boolean = o.name == fish.name && o.count == fish.count && o.size.length == fish.length && o.size.weight == fish.weight
    def add(photo: Photo): FishSize = FishSize.addNew(photo, fish.name, fish.count, fish.weight, fish.length)
  }
  implicit class PhotoFile(file: Storage.S3File) {
    def asPhoto: Option[Photos] = {
      def resize(src: Image, image: ScrImage, max: Int, relation: ImageRelation.Relation.Value) = {
        val rotated = allCatch.opt {
          val metadata = ImageMetadataReader.readMetadata(src.file.read)
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
          case name :: parent :: left => List("photo", Image.Kind.REDUCED.toString, parent, relation.toString).mkString("/")
          case _                      => throw new IllegalArgumentException(f"Unexpected file path: ${file.paths}")
        }
        val dst = Image.addNewWithWriter(scaled.writer(Format.JPEG).write, path, scaled.width, scaled.height, Image.Kind.REDUCED)
        ImageRelation.addNew(src, dst, relation)
        dst
      }
      for {
        i <- allCatch.opt { Option(ScrImage(file.read)) }.flatten
        o <- allCatch opt Image.addNewWithFile(file.path, i.width, i.height)
        m <- allCatch opt resize(o, i, Settings.Image.sizeMainview, ImageRelation.Relation.MAIN_VIEW)
        t <- allCatch opt resize(o, i, Settings.Image.sizeThumbnail, ImageRelation.Relation.THUMBNAIL)
      } yield Photos(o, m, t)
    }
  }
  implicit class PhotoGroup(photo: Photo) {
    def group: Option[Photos] = {
      for {
        o <- photo.image
        m <- ImageRelation.findBy(o, ImageRelation.Relation.MAIN_VIEW).flatMap(_.imageDst).headOption
        t <- ImageRelation.findBy(o, ImageRelation.Relation.THUMBNAIL).flatMap(_.imageDst).headOption
      } yield Photos(o, m, t)
    }
  }

  val TicketExpired = BadRequest("Ticket Expired")
  val SessionExpired = BadRequest("Session Expired")
}
