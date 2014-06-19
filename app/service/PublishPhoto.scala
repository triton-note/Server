package service

import java.util.Date
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import play.api.Logger
import akka.actor._
import akka.pattern._
import akka.util._
import models._
import models.db._
import Facebook._

object PublishPhoto {
  /**
   * Name of Album on FaceBook
   */
  val albumName: String = System.getenv("ALBUM_NAME")
  def publish(fishes: List[FishSize])(implicit accessKey: AccessKey): Future[List[ObjectId]] = {
    val photos = fishes.groupBy(_.photo)
    Logger.info(f"Publishing ${photos.keys.mkString(", ")}")
    val all = for {
      photo <- photos.keys.flatten.toList
      image <- photo.image
      report <- photo.catchReport
      user <- report.user
      fishSizes = photos(Some(photo))
    } yield {
      val comment = {
        val records = fishSizes.map { fish =>
          val length = fish.length.map(v => f"$v%.2f ${user.lengthUnit}")
          val weight = fish.weight.map(v => f"$v%.2f ${user.weightUnit}")
          val value = List(length, weight).flatten.mkString(", ") match {
            case "" => ""
            case v  => f"($v)"
          }
          val count = fish.count match {
            case 1 => ""
            case n => f"x $n"
          }
          f"${fish.name} $count $value"
        }
        val msg = report.comments.headOption match {
          case Some(m) => "\n\n" + m
          case None    => ""
        }
        records.mkString("\n") + msg
      }
      Fishing.publish(List(image.file), Some(comment))
    }
    Future.sequence(all).map(_.flatten)
  }
}
