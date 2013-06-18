package service

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
    val list = find(all.toList).keys.map(_.id).toList
    import AlbumManager._
    list.distinct.foreach { album =>
      albums ! Msg.Refresh(album, timer, accessKey, all.toList)
    }
  }
  def find(list: List[PreInfo]): Map[Album, List[(Photo, Option[String])]] = withTransaction {
    val map = for {
      info <- list
      id <- info.committed
      photo <- Photo.getById(id)
      sub <- info.submitted
      album <- photo.findAlbum(sub.grounds, sub.date)
    } yield {
      val comment = if (sub.comment == null || sub.comment.size <= 0) None else Some(sub.comment)
      (album, (photo, comment))
    }
    for {
      (album, list) <- map.groupBy(_._1)
    } yield (album, list.map(_._2))
  }
  def publish(album: Album, photos: List[(Photo, Option[String])])(implicit accessKey: AccessKey) = {
    def addAll(albumId: ObjectId, photos: List[(Photo, Option[String])]) = for {
      (photo, comment) <- photos
      image <- photo.image
    } yield Publish.addPhoto(albumId)(image.file, comment)
    for {
      albumId <- Publish getAlbumOrCreate nameOf(album)
      r <- Future sequence addAll(albumId, photos)
    } yield r
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
          if (list.size <= find(list).size) self ! Msg.Publish(albumId)
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
          committed = find(list)
          (album, photos) <- committed.find(_._1.id == albumId)
        } publish(album, photos)(key)
        stay using Data.Store(map - albumId)
      }
    }
  }
}
