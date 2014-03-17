package models

import scala.util.control.Exception._
import scala.xml._
import play.api.Logger
import java.util.Date
import db._

object PreInfo {
  import javax.xml.transform.stream._
  import javax.xml.validation._
  implicit def stringToSource(text: String) = new StreamSource(new java.io.StringReader(text))
  val XSD: Elem = {
    val name = getClass.getName.split(".").last
    XML load getClass.getResourceAsStream(name)
  }
  /**
   * Schema of XSD for validation
   */
  private val schema = {
    val schemaLang = "http://www.w3.org/2001/XMLSchema"
    val factory = SchemaFactory.newInstance(schemaLang)
    factory.newSchema(XSD.toString)
  }
  /**
   * Validate XML
   */
  private def validate(string: String): Option[String] = allCatch opt {
    schema.newValidator().validate(string)
    string
  }
  /**
   * Parse XML and into PreInfo objects
   */
  private def load(xml: Node): Option[PreInfo] = {
    import RichXML._
    def makeGeoInfo(x: Node) = {
      for {
        geo <- (x \ "geoinfo").headOption
        latitude <- geo \@% "latitude"
        longitude <- geo \@% "longitude"
      } yield GeoInfo(latitude, longitude)
    }
    def makeFishes(x: Node) = {
      for {
        fish <- (x \ "fish").toList
        name <- fish \@ "name"
        count <- fish \@# "count"
        length = fish \@% "length"
        weight = fish \@% "widht"
      } yield Fish(name, count, length, weight)
    }
    val basic = {
      for {
        filepath <- xml \@ "filepath"
        uploaded = xml \@# "uploaded"
        format <- xml \@ "format"
        width <- xml \@# "width"
        height <- xml \@# "height"
        timestamp = (xml \@ "timestamp").map(dateFormat.parse)
        geoinfo = makeGeoInfo(xml)
      } yield BasicInfo(uploaded, filepath, format, width, height, timestamp, geoinfo)
    }
    val inference = {
      for {
        x <- (xml \ "inference").headOption
        date <- (x \@ "date").map(dateFormat.parse)
        spots = (x \ "spot").toList.flatMap(_ \@ "name")
        fishes = makeFishes(x)
      } yield InferentialInfo(date, spots, fishes)
    }
    val submission = {
      for {
        x <- (xml \ "submission").headOption
        spot <- x \@ "spot"
        date <- (x \@ "date").map(dateFormat.parse)
        geoinfo <- makeGeoInfo(x)
        fishes = makeFishes(x)
        comment <- (x \ "comment").headOption.map(_.text)
      } yield SubmittedInfo(date, spot, geoinfo, fishes, comment)
    }
    basic.map(PreInfo(_, inference, submission))
  }
  def apply(xml: Node): Option[PreInfo] = apply(xml.toString)
  /**
   * Find PreInfo in the given XML.
   */
  def apply(xml: String): Option[PreInfo] = {
    Logger.trace(f"Loading PreInfo from XML: $xml")
    for {
      string <- validate(xml)
      info <- load(scala.xml.XML loadString string)
    } yield info
  }
  case class BasicInfo private[PreInfo] (uploaded: Option[Long], filepath: String, format: String, width: Long, height: Long, timestamp: Option[Date], geoinfo: Option[GeoInfo])
  case class SubmittedInfo private[PreInfo] (date: Date, spot: String, geoinfo: GeoInfo, fishes: List[Fish], comment: String)
  case class InferentialInfo private[PreInfo] (date: Date, spots: List[String], fishes: List[Fish])
  case class Fish(name: String, count: Long, length: Option[Double], weight: Option[Double])
}
case class PreInfo(basic: PreInfo.BasicInfo,
                   inference: Option[PreInfo.InferentialInfo],
                   submission: Option[PreInfo.SubmittedInfo]) {
  import PreInfo._
  def toXML: Node = {
    implicit def fromBoolean(v: Boolean) = v.toString
    implicit def fromDouble(v: Double) = f"$v%1.10f"
    implicit def fromDoubleO(o: Option[Double]) = o.map(v => f"$v%1.10f").orNull
    implicit def fromLong(v: Long) = f"$v%d"
    implicit def fromLongO(o: Option[Long]) = o.map(v => f"$v%d").orNull
    implicit def fromDate(v: Date) = dateFormat.format(v)
    implicit def fromDateO(o: Option[Date]) = o.map(dateFormat.format).orNull
    def xmlFishes(fishes: List[Fish]) = fishes map { fish =>
      <fish name={ fish.name } count={ fish.count } length={ fish.length } weight={ fish.weight }/>
    }
    def xmlGeoInfo(geoinfos: GeoInfo*) = geoinfos map { g =>
      <geoinfo latitude={ g.latitude } longitude={ g.longitude }/>
    }
    <file uploaded={ basic.uploaded } filepath={ basic.filepath } format={ basic.format } width={ basic.width } height={ basic.height } timestamp={ basic.timestamp }>
      {
        xmlGeoInfo(basic.geoinfo.toSeq: _*)
      }{
        inference.toSeq map { i =>
          <inference date={ i.date }>
            {
              i.spots map { spot =>
                <spot name={ spot }/>
              }
            }{
              xmlFishes(i.fishes)
            }
          </inference>
        }
      }{
        submission.toSeq map { s =>
          <submission date={ s.date } spot={ s.spot }>
            {
              xmlGeoInfo(s.geoinfo)
            }{
              xmlFishes(s.fishes)
            }
            <comment>{ s.comment }</comment>
          </submission>
        }
      }
    </file>
  }
  def infer(date: Date, spots: List[String], fishes: List[Fish]): PreInfo = {
    val i = InferentialInfo(date, spots, fishes)
    copy(inference = Some(i))
  }
  def upload(file: java.io.File): PreInfo = if (basic.uploaded.nonEmpty) this else {
    Logger.trace(f"Uploaded file(${file.getName})")
    Images.addNew(file.length, basic.width, basic.height, basic.format) match {
      case Some(image) if (image.file write file) => copy(basic = basic.copy(uploaded = Some(image.id)))
      case _                                      => this
    }
  }
  def submit(date: Date, spot: String, geoinfo: GeoInfo, comment: String, fishes: List[Fish]): PreInfo = {
    val s = SubmittedInfo(date, spot, geoinfo, fishes, comment)
    Logger.trace(f"Submitting $s")
    copy(submission = Some(s))
  }
  def commit(user: User): Option[List[FishSize]] = {
    for {
      s <- submission
      imageId <- basic.uploaded
      image <- Images.get(imageId)
      report <- CatchReports.addNew(user, s.geoinfo, s.date)
      photo <- Photos.addNew(report, image)
    } yield {
      Logger.debug(f"Saving to DataBase $this")
      if (s.comment.length > 0) Comments.addNew(user, report, s.comment)
      s.fishes.flatMap { f =>
        FishSizes.addNew(photo, f.name, f.count, f.weight, f.length)
      }
    }
  }
}