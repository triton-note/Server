package service

import scala.concurrent._
import ExecutionContext.Implicits.global
import java.util.Date
import models._

object InferencePreInfo {
  def initialize(vt: db.VolatileToken, node: scala.xml.Node): Future[List[PreInfo]] = {
    val xml = PreInfo.asXML(PreInfo load node)
    Future {
      inference(vt setExtra xml)
    }
  }
  def submitByUser(vt: db.VolatileToken)(filepath: String, date: Date, grounds: String, comment: String)(implicit user: db.User): Future[Option[PreInfo]] = {
    Future {
      for {
        xml <- vt.extra
        p <- (PreInfo read xml).find(_.basic.filepath == filepath)
      } yield {
        val submitted = p.submit(date, grounds, comment)
        // Refresh inferential info
        submitted
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
  def inference(vt: db.VolatileToken): List[PreInfo] = {
    val all = vt.extra.toList.flatMap(PreInfo.read)
    val next = all.map { info =>
      info.submitted match {
        case Some(_) => info
        case None => {
          // Inference
          info
        }
      }
    }
    update(vt, next: _*)
  }
}
