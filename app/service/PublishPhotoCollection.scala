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
      albums ! Msg.Refresh(albumId, timer, accessKey, all.toList)
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
  def publish(album: Album, photos: Map[Photo, Option[String]])(implicit accessKey: AccessKey): Future[List[ObjectId]] = {
    def addAll(albumIdOpt: Option[ObjectId], photos: List[(Photo, Option[String])]) = for {
      (photo, comment) <- photos
      albumId <- albumIdOpt
      image <- photo.image
    } yield Publish.addPhoto(albumId)(image.file, comment)
    for {
      albumId <- Publish getAlbumOrCreate nameOf(album)
      list <- Future sequence addAll(albumId, photos.toList)
    } yield list.flatten
  }
  /**
   * Management of timer and collection photo list of album
   */
  object AlbumManager {
    val albums = actorSystem.actorOf(Props[AlbumManager])
    sealed trait Data
    object Data {
      case class Store(map: Map[Long, (AccessKey, List[PreInfo])]) extends Data
    }
    sealed trait State
    object State {
      case object Running extends State
    }
    sealed trait Msg
    object Msg {
      case class Refresh(albumId: Long, timer: FiniteDuration, accessKey: AccessKey, list: List[PreInfo]) extends Msg
      case class Publish(albumId: Long) extends Msg
    }
  }
  class AlbumManager extends Actor with FSM[AlbumManager.State, AlbumManager.Data] {
    import AlbumManager._
    startWith(State.Running, Data.Store(Map()))
    when(State.Running) {
      case Event(Msg.Refresh(albumId, timer, accessKey, list), Data.Store(map)) => map.get(albumId) match {
        case Some(_) => {
          if (list.size <= findCommitted(list).size) self ! Msg.Publish(albumId)
          stay using Data.Store(map.updated(albumId, (accessKey, list)))
        }
        case None => {
          actorSystem.scheduler.scheduleOnce(timer, self, Msg.Publish(albumId))
          stay using Data.Store(map + (albumId -> (accessKey, list)))
        }
      }
      case Event(Msg.Publish(albumId), Data.Store(map)) => {
        for {
          (key, list) <- map.get(albumId)
          album <- Album get albumId
        } {
          val a = withTransaction {
            for {
              (info, photo) <- findCommitted(list)
              (date, grounds, comment) <- info.submitted match {
                case Some(s) => Some(s.date, s.grounds, Some(s.comment))
                case None => info.inference match {
                  case Some(i) => Some(i.date, i.grounds, None)
                  case None    => None
                }
              }
              a <- photo.findAlbum(grounds, date)
              if (a.id == albumId)
            } yield (photo, comment)
          }
          publish(album, a)(key)
        }
        stay using Data.Store(map - albumId)
      }
    }
  }
}
