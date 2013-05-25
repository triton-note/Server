package models

import play.api.Logger
import scala.concurrent._
import ExecutionContext.Implicits.global
import java.util.Date
import java.io.File

object UncommittedPhoto {
  implicit class hasNullable[T >: Null](a: T) {
    def ? = if (a == null) None else Some(a)
  }
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
    def load(xml: Node): Option[List[PreInfo]] = {
      validate(xml.toString) map { _ =>
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
          PreInfo(BasicInfo(filepath, format, width.round, height.round, timestamp, geoinfo))
        }
      }
    }
    /**
     * Find PreInfo in the given VolatileToken.
     * if filepath is specified, return only PreInfo which match filepath.
     */
    def load(vt: db.VolatileToken, filepath: String = null): List[PreInfo] = {
      for {
        ex <- vt.extra.toList
        string <- validate(ex).toList
        list <- load(scala.xml.XML loadString string).toList
        info <- list
        if (filepath == null || info.basic.filepath == filepath)
      } yield info
    }
    def asXML(infos: List[PreInfo]) = {
      <files>{
        infos map { info =>
          val b = info.basic
          <file filepath={ b.filepath } timestamp={ b.timestamp.map(df.format) getOrElse "" }>
            b.geoinfo.map{ g: GeoInfo =>
              implicit def ds(d: Double) = f"$d%1.10f"
              <geoinfo latitude={ g.latitude } longitude={ g.longitude }/>
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
  }
  case class PreInfo(basic: PreInfo.BasicInfo, submitted: Option[PreInfo.SubmittedInfo] = None, committed: Option[Long] = None) {
    def commit(alias: db.UserAlias, file: File): Option[PreInfo] = committed match {
      case Some(id) => None
      case None => submitted.map { s =>
        import db._
        withTransaction {
          implicit val user = alias.user
          val album = AlbumOwner.create(user, s.date, s.grounds)
          val photo = Photo.addNew(basic.geoinfo, basic.timestamp.map(a => a)) bindTo user bindTo album add s.comment
          val data = PhotoData.addNew(store(file).path, "original", photo, basic.format, file.length, basic.width, basic.height)
          copy(committed = Some(photo.id))
        }
      }
    }
    def update(date: java.sql.Timestamp, grounds: String, comment: String)(implicit user: db.User): PreInfo = committed match {
      case None => {
        val s = PreInfo.SubmittedInfo(date, grounds, comment)
        copy(submitted = Some(s))
      }
      case Some(id) => {
        val photo = db.Photo.getById(id).get
        val p = photo add adding.comment
      }
    }
  }
  def update(vt: db.VolatileToken, filepath: String, date: java.sql.Timestamp, grounds: String, comment: String)(implicit user: db.User) = {
    val infos = PreInfo load vt
    for {
      info <- infos.find(_.basic.filepath == filepath)
    } yield {
      val next = info.update(date, grounds, comment)
      val list = next :: infos.filter(_.basic.filepath != filepath)
      vt setExtra PreInfo.asXML(list)
    }
  }
  /**
   * Just save to Storage
   */
  def store(src: File): Storage.S3File = {
    val unique = play.api.libs.Codecs.sha1(System.currentTimeMillis.toString)
    val s3 = Storage.file(unique, src.getName)
    val stored = s3 write src
    Logger.debug(f"Stored (${stored}) $s3")
    s3
  }
  def inference(vt: db.VolatileToken) {
  }
}
