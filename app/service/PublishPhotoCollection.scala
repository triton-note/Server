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
  def add(all: PreInfo*)(implicit accessKey: AccessKey, timer: FiniteDuration) = {
    val list = withTransaction {
      for {
        (info, photo) <- findCommitted(all.toList).toList
        sub <- info.submitted
        album <- photo.findAlbum(sub.grounds, sub.date)
      } yield album.id
    }
    import AlbumManager._
    list.distinct.foreach { albumId =>
      albums ! Msg.Refresh(albumId, all.toList, accessKey, timer)
    }
  }
  def findCommitted(list: List[PreInfo]): Map[PreInfo, Photo] = {
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
      def submittedOrInference(info: PreInfo) = info.submitted match {
        case Some(s) => Some(s.date, s.grounds, Some(s.comment))
        case None => info.inference match {
          case Some(i) => Some(i.date, i.grounds, None)
          case None    => None
        }
      }
      def publishAll = withTransaction {
        (Album get albumId).map(nameOf).map(publish(_)_) map {
          _(
            for {
              (info, photo) <- findCommitted(photos).toList
              image <- photo.image
              (date, grounds, comment) <- submittedOrInference(info)
              a <- photo.findAlbum(grounds, date)
              if (a.id == albumId)
            } yield (image.file, comment)
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
      case class Refresh(albumId: Long, list: List[PreInfo], accessKey: AccessKey, timer: FiniteDuration) extends Msg
      case class Publish(albumId: Long) extends Msg
    }
  }
  class AlbumManager extends Actor with FSM[AlbumManager.State, AlbumManager.Data] {
    import AlbumManager._
    startWith(State.Running, Data.Store(Map()))
    when(State.Running) {
      case Event(Msg.Refresh(albumId, list, accessKey, timer), Data.Store(map)) => {
        val timerName = f"Album-${albumId}%d"
        cancelTimer(timerName)
        setTimer(timerName, Msg.Publish(albumId), timer, false)
        val o = AlbumPhotos(albumId, list, accessKey)
        stay using Data.Store(map + (albumId -> o))
      }
      case Event(Msg.Publish(albumId), Data.Store(map)) => {
        (map get albumId) foreach (_.publishAll)
        stay using Data.Store(map - albumId)
      }
    }
  }
}
