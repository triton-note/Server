import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import play.api.Logger
import play.api.libs.json._

import org.fathens.play.util.Exception.allCatch

import com.sksamuel.scrimage.{Format, Image => ScrImage}

import models.{Report, Settings}
import models.db.{FishSize, Image, ImageRelation, Photo, User, VolatileToken}

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
    def asURL = Report.Photo(original.url.toString, mainview.url.toString, thumbnail.url.toString)
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
    def same(o: FishSize): Boolean = o.name == fish.name && o.count == fish.count && o.length == fish.length.map(_.tupled) && o.weight == fish.weight.map(_.tupled)
    def add(photo: Photo): FishSize = FishSize.addNew(photo, fish.name, fish.count, fish.weight.map(_.tupled), fish.length.map(_.tupled))
  }
  implicit class PhotoFile(file: java.io.File) {
    def asPhoto: Option[Photos] = {
      def original(image: ScrImage) = {
        val dst = Image.addNew(image.width, image.height)
        dst.file write file
        dst
      }
      def resize(src: Image, image: ScrImage, max: Int, relation: ImageRelation.Relation.Value) = {
        val (width, height) = (image.width, image.height)
        val (w, h) = if (width > height) (max, height * max / width) else (width * max / height, max)
        Logger debug f"Resizing image for ${relation}: (${width} x ${height}) -> (${w} x ${h})"
        val scaled = image.scaleTo(w, h)
        val dst = Image.addNew(scaled.width, scaled.height, Image.Kind.REDUCED)
        val out = new ByteArrayOutputStream()
        scaled.writer(Format.JPEG).write(out)
        dst.file.slurp(new ByteArrayInputStream(out.toByteArray))
        ImageRelation.addNew(src, dst, relation)
        dst
      }
      for {
        io <- allCatch opt ScrImage(file)
        i <- Option(io)
        o <- allCatch opt original(i)
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
}
