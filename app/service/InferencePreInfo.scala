package service

import scala.concurrent._
import ExecutionContext.Implicits.global
import java.util.Date
import models._

object InferencePreInfo {
  implicit class InferentialPreInfo(info: PreInfo) {
    def isNearTo(o: PreInfo) = {
      val r = for {
        a <- info.basic.geoinfo
        b <- o.basic.geoinfo
        d = a distanceTo b
        if (d < 1000)
      } yield true
      r getOrElse false
    }
  }
  def initialize(vt: db.VolatileToken, node: scala.xml.Node): Future[List[PreInfo]] = {
    val xml = PreInfo.asXML(PreInfo load node)
    inference(vt setExtra xml)
  }
  def submitByUser(vt: db.VolatileToken)(filepath: String, date: Date, grounds: String, comment: String)(implicit user: db.User): Future[Option[PreInfo]] = {
    Future {
      db.withTransaction {
        for {
          xml <- vt.extra
          list = PreInfo read xml
          p <- list.find(_.basic.filepath == filepath)
          s <- p.submit(date, grounds, comment)
        } yield {
          val n = PreInfo.inference(date, grounds)
          // Refresh inferential info
          val r = list.map { info =>
            if (info != s &&
              info.submitted.isEmpty &&
              (info isNearTo s)) {
              info.copy(inference = Some(n))
            } else info
          }
          vt.setExtra(PreInfo asXML r)
          s
        }
      }
    }
  }
  def commitByUpload(vt: db.VolatileToken)(filename: String, file: java.io.File)(implicit user: db.User): Future[Option[PreInfo]] = {
    Future {
      for {
        v <- vt.refresh
        xml <- v.extra
        info <- (PreInfo read xml).find(_.basic.filepath == filename)
        c <- info.commit(file)
      } yield {
        update(v, c)
        info
      }
    }
  }
  /**
   * Update info which stored in token.
   */
  private def update(vt: db.VolatileToken, infos: PreInfo*): List[PreInfo] = {
    val except = for {
      xml <- vt.extra.toList
      i <- PreInfo read xml
      if infos.find(_.basic.filepath == i.basic.filepath).isDefined
    } yield i
    val next = infos.toList ::: except
    vt setExtra PreInfo.asXML(next)
    next
  }
  /**
   * Inference of Date and Grounds.
   * This should finish before submission.
   */
  def inference(vt: db.VolatileToken): Future[List[PreInfo]] = Future {
    db.withTransaction {
      val all = for {
        r <- vt.refresh.toList
        xml <- r.extra.toList
        info <- PreInfo read xml
      } yield info
      val next = all.map { info =>
        info.inference match {
          case Some(_) => info
          case None => {
            val inf = PreInfo.inference(db.currentTimestamp, "")
            // TODO Inference by referencing all other PreInfos
            info.copy(inference = Some(inf))
          }
        }
      }
      update(vt, next: _*)
    }
  }
}
