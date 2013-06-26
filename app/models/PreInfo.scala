package models

import play.api.Logger
import java.util.Date
import db._

object PreInfo {
  import javax.xml.transform.stream._
  import javax.xml.validation._
  import scala.xml._
  val df = new java.text.SimpleDateFormat("yyyy-MM-dd'T'h:m:ss")
  implicit def stringToSource(text: String) = new StreamSource(new java.io.StringReader(text))
  val XSD = <?xml version="1.0"?>
            <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
              <xs:element name="files">
                <xs:complexType>
                  <xs:element name="file" minOccurs="1" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:attribute name="filepath" type="xs:string" use="required"/>
                      <xs:attribute name="format" type="xs:string" use="required"/>
                      <xs:attribute name="width" type="xs:long" use="required"/>
                      <xs:attribute name="height" type="xs:long" use="required"/>
                      <xs:attribute name="timestamp" type="xs:dateTime"/>
                      <xs:element name="geoinfo" minOccurs="0" maxOccurs="1">
                        <xs:complexType>
                          <xs:attribute name="latitude" type="xs:double" use="required"/>
                          <xs:attribute name="longitude" type="xs:double" use="required"/>
                        </xs:complexType>
                      </xs:element>
                      <xs:element name="inference" minOccurs="0" maxOccurs="1">
                        <xs:complexType>
                          <xs:attribute name="grounds" type="xs:string" use="required"/>
                          <xs:attribute name="date" type="xs:date" use="required"/>
                        </xs:complexType>
                      </xs:element>
                      <xs:element name="info" minOccurs="0" maxOccurs="1">
                        <xs:complexType>
                          <xs:attribute name="grounds" type="xs:string" use="required"/>
                          <xs:attribute name="date" type="xs:date" use="required"/>
                          <xs:element name="comment" type="xs:string" minOccurs="1" maxOccurs="1"/>
                        </xs:complexType>
                      </xs:element>
                      <xs:element name="committed" minOccurs="0" maxOccurs="1">
                        <xs:complexType>
                          <xs:attribute name="id" type="xs:string" use="required"/>
                        </xs:complexType>
                      </xs:element>
                    </xs:complexType>
                  </xs:element>
                </xs:complexType>
              </xs:element>
            </xs:schema>
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
  def validate(string: String): Option[String] = {
    try {
      schema.newValidator().validate(string)
      Some(string)
    } catch {
      case ex: Exception => None
    }
  }
  /**
   * Parse XML and into PreInfo objects
   */
  def load(xml: Node): List[PreInfo] = {
    implicit class HasAtt(xml: scala.xml.Node) {
      def \@(name: String) = (xml \ f"@$name").headOption.map(_.toString)
      def \#(name: String) = \@(name).map(_.toDouble)
    }
    for {
      info <- (xml \ "file").toList
      filepath <- info \@ "path"
      format <- info \@ "format"
      width <- info \# "width"
      height <- info \# "height"
    } yield {
      val timestamp = (info \@ "timestamp").map(df.parse)
      val geoinfo = for {
        geo <- (info \ "geoinfo").headOption
        latitude <- geo \# "latitude"
        longitude <- geo \# "longitude"
      } yield GeoInfo(latitude, longitude)
      PreInfo(
        BasicInfo(filepath, format, width.round, height.round, timestamp, geoinfo),
        {
          for {
            i <- (info \ "inference").headOption
            d <- (i \@ "date").map(df.parse)
            g <- i \@ "grounds"
          } yield InferentialInfo(d, g)
        },
        {
          for {
            i <- (info \ "info").headOption
            d <- (i \@ "date").map(df.parse)
            g <- i \@ "grounds"
            c <- i \@ "comment"
          } yield SubmittedInfo(d, g, c)
        },
        {
          for {
            c <- (info \ "committed").headOption
            id <- c \# "id"
          } yield id.toLong
        }
      )
    }
  }
  /**
   * Find PreInfo in the given XML.
   */
  def read(xml: String): List[PreInfo] = {
    for {
      string <- validate(xml).toList
      info <- load(scala.xml.XML loadString string).toList
    } yield info
  }
  def asXML(infos: List[PreInfo]): scala.xml.Elem = {
    <files>{
      infos map { info =>
        val b = info.basic
        <file filepath={ b.filepath } timestamp={ b.timestamp.map(df.format) getOrElse "" }>
          b.geoinfo.map{ g: GeoInfo =>
            implicit def ds(d: Double) = f"$d%1.10f"
            <geoinfo latitude={ g.latitude } longitude={ g.longitude }/>
          }
          info.inference.map{ i: InferentialInfo =>
            <inference date={ df format i.date } grounds={ i.grounds }/>
          }
          info.submitted.map{ s: SubmittedInfo =>
            <submitted date={ df format s.date } grounds={ s.grounds } comment={ s.comment }/>
          }
          info.committed.map{ c: Long =>
            <committed id={ c.toString }/>
          }
        </file>
      }
    }</files>
  }
  case class BasicInfo(filepath: String, format: String, width: Long, height: Long, timestamp: Option[Date], geoinfo: Option[GeoInfo])
  case class SubmittedInfo(date: Date, grounds: String, comment: String)
  case class InferentialInfo(date: Date, grounds: String)
}
case class PreInfo(basic: PreInfo.BasicInfo,
                   inference: Option[PreInfo.InferentialInfo],
                   submitted: Option[PreInfo.SubmittedInfo],
                   committed: Option[Long]) {
  import PreInfo._
  def commit(file: java.io.File)(implicit user: User): Option[PreInfo] = committed match {
    case Some(id) => None
    case None => submitted.map { s =>
      withTransaction {
        val album = AlbumOwner.create(user, s.date, s.grounds)
        val photo = Photo.addNew(basic.geoinfo, basic.timestamp.map(a => a)) bindTo user bindTo album add s.comment
        val data = Image.addNew(Image.KIND_ORIGINAL, photo, basic.format, file.length, basic.width, basic.height)
        data.file write file
        copy(committed = Some(photo.id))
      }
    }
  }
  def submit(date: Date, grounds: String, comment: String)(implicit user: User): PreInfo = committed match {
    case None => {
      val s = SubmittedInfo(date, grounds, comment)
      copy(submitted = Some(s))
    }
    case Some(id) => withTransaction {
      val photo = Photo.getById(id).get add comment
      val a = submitted.get.copy(comment = comment)
      val b = a.copy(date = date, grounds = grounds)
      if (a != b) {
        def findAlbum(s: SubmittedInfo) = photo.findAlbum(s.grounds, s.date)
        findAlbum(b) match {
          case None           => db.Album.addNew(b.date, b.grounds)
          case Some(newAlbum) => Logger.info(f"The new album (id=${newAlbum.id}) has already been associated with this photo.")
        }
        findAlbum(a) match {
          case None           => Logger.warn("The old album is not associated with this photo.")
          case Some(oldAlbum) => if (oldAlbum.photos.isEmpty) oldAlbum.delete
        }
      }
      copy(submitted = Some(b))
    }
  }
}