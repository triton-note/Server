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
                        <xs:attribute name="timestamp" type="xs:dateTime" use="required"/>
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
        } yield {
          val timestamp = (info \@ "timestamp").map(df.parse)
          val geoinfo = for {
            latitude <- info \# "latitude"
            longitude <- info \# "longitude"
          } yield GeoInfo(latitude, longitude)
          PreInfo(filepath, timestamp, geoinfo)
        }
      }
    }
    def load(vt: db.VolatileToken, filepath: String = null): List[PreInfo] = {
      for {
        ex <- vt.extra.toList
        string <- validate(ex).toList
        list <- load(scala.xml.XML loadString string).toList
        info <- list
        if (filepath == null || info.filepath == filepath)
      } yield info
    }
    def asXML(infos: List[PreInfo]) = {
      <files>{
        infos map { info =>
          implicit def ds(v: Option[Double]) = v map { d => f"$d%1.10f" } getOrElse ""
          <file filepath={ info.filepath } timestamp={ df.format(info.timestamp) } latitude={ info.geoinfo.map(_.latitude) } longitude={ info.geoinfo.map(_.longitude) }/>
        }
      }</files>
    }
  }
  case class PreInfo(filepath: String, timestamp: Option[Date], geoinfo: Option[GeoInfo],
                     grounds: Option[String] = None, date: Option[Date] = None, comment: Option[String] = None,
                     committed: Option[Long] = None) {
    def commit(user: db.UserAlias, file: File): PreInfo = { /*
    val album = db.Album.addNew(Some(adding.date), Some(adding.grounds))
    val photo = db.Photo.addNew(file.path) bindTo user bindTo album add adding.comment
    Logger.info("Saved photo: %s".format(photo))*/
      null
    }
  }
  /**
   * Just save to Storage
   */
  def store(src: File, file: Storage.S3File): Storage.S3File = {
    val stored = file write src
    Logger.debug(f"Stored ($stored) file: $file")
    file
  }
  def inference(vt: db.VolatileToken) {
  }
}
