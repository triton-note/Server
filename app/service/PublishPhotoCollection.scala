package service

import java.util.Date
import scala.concurrent._
import scala.concurrent.duration._
import akka.actor._
import akka.pattern._
import akka.util._
import models._
import models.db._
import Facebook._

object PublishPhotoCollection {
  val actorSystem = ActorSystem("PublishToFacebook")
  import ExecutionContext.Implicits.global
  implicit val timeout = Timeout(1 minutes)
  implicit class PhotoImage(photo: Photo) {
    def image = Image.findBy(photo).filter(_.kind == Image.KIND_ORIGINAL).sortBy(_.createdAt.getTime).headOption
  }
  val dateFormat = new java.text.SimpleDateFormat("yyyy-MM-dd")
  /**
   * Making name of Album
   */
  def nameOf(album: Album): String = {
    f"${dateFormat.format(album.date)} ${album.grounds}"
  }
  /**
   * Facade for adding photo to album
   */
  def add(all: PreInfo*)(implicit accessKey: AccessKey, timer: FiniteDuration) = sendBy(all) { (albumId, infos) =>
    AlbumManager.Msg.Add(albumId, infos, accessKey, timer)
  }
  def cancel(all: PreInfo*) = sendBy(all) { (albumId, infos) =>
    AlbumManager.Msg.Update(albumId, infos.filterNot(all.contains))
  }
  private def sendBy(all: Seq[PreInfo])(m: (Long, List[PreInfo]) => AlbumManager.Msg) = {
    val list = withTransaction {
      for {
        (info, photo) <- findCommitted(all).toList
        sub <- info.submitted
        album <- photo.findAlbum(sub.grounds, sub.date)
      } yield album.id -> info
    }
    list.groupBy(_._1).map {
      case (k, v) => m(k, v.map(_._2))
    }.foreach(AlbumManager.albums ! _)
  }
  def findCommitted(list: Seq[PreInfo]): Map[PreInfo, Photo] = {
    val map = for {
      info <- list
      id <- info.committed
      photo <- Photo get id
    } yield (info, photo)
    map.toMap
  }
  def publish(albumName: String)(photos: (Storage.S3File, Option[String])*)(implicit accessKey: AccessKey): Future[List[ObjectId]] = {
    def addAll(albumIdOpt: Option[ObjectId]) = for {
      albumId <- albumIdOpt.toList
      (imageFile, comment) <- photos
    } yield Publish.addPhoto(albumId)(imageFile, comment)
    for {
      albumId <- Publish getAlbumOrCreate albumName
      list <- Future sequence addAll(albumId)
    } yield list.flatten
  }
  /**
   * Management of timer and collection photo list of album
   */
  object AlbumManager {
    case class AlbumPhotos(albumId: Long, photos: List[PreInfo], accessKey: AccessKey) {
      implicit val facebookKey = accessKey
      def publishAll = withTransaction {
        (Album get albumId).map(nameOf).map(publish(_)_) map {
          _(
            for {
              (info, photo) <- findCommitted(photos).toList
              image <- photo.image
              soi <- info.soi
              a <- photo.findAlbum(soi.grounds, soi.date)
              if (a.id == albumId)
            } yield (image.file, soi.comment)
          )
        }
      }
    }
    val albums = actorSystem.actorOf(Props[AlbumManager])
    sealed trait Data
    object Data {
      case class Store(map: Map[Long, AlbumPhotos]) extends Data
    }
    sealed trait State
    object State {
      case object Running extends State
    }
    sealed trait Msg
    object Msg {
      case class Add(albumId: Long, list: List[PreInfo], accessKey: AccessKey, timer: FiniteDuration) extends Msg
      case class Update(albumId: Long, list: List[PreInfo]) extends Msg
      case class Publish(albumId: Long) extends Msg
    }
  }
  class AlbumManager extends Actor with FSM[AlbumManager.State, AlbumManager.Data] {
    import AlbumManager._
    startWith(State.Running, Data.Store(Map()))
    when(State.Running) {
      case Event(Msg.Add(albumId, list, accessKey, timer), Data.Store(map)) => {
        timerCancel(albumId)
        val next = if (list.isEmpty) map - albumId else {
          timerStart(albumId, timer)
          map + (albumId -> AlbumPhotos(albumId, list, accessKey))
        }
        stay using Data.Store(next)
      }
      case Event(Msg.Update(albumId, list), Data.Store(map)) => {
        if (!(map contains albumId)) stay else {
          val next = if (list.isEmpty) {
            timerCancel(albumId)
            map - albumId
          } else map + (albumId -> map(albumId).copy(photos = list))
          stay using Data.Store(next)
        }
      }
      case Event(Msg.Publish(albumId), Data.Store(map)) => {
        (map get albumId) foreach (_.publishAll)
        stay using Data.Store(map - albumId)
      }
    }
    def timerOf(albumId: Long) = f"Album-${albumId}%d"
    def timerCancel(albumId: Long) = cancelTimer(timerOf(albumId))
    def timerStart(albumId: Long, timer: FiniteDuration) = setTimer(timerOf(albumId), Msg.Publish(albumId), timer, false)
  }
}
