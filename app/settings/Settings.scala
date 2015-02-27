package settings

import scala.concurrent.duration._

import play.api.libs.json._

/**
 * Definition of Settings Model
 */
case class Settings(
  tokenTimeout: Settings.TokenTimeout,
  image: Settings.Image,
  facebook: Settings.Facebook,
  openweathermap: Settings.OpenWeatherMap)
object Settings {
  case class TokenTimeout(ticket: FiniteDuration, session: FiniteDuration)
  object TokenTimeout {
    implicit val json = Json.format[TokenTimeout]
  }
  case class Image(urlTimeout: FiniteDuration, size: Image.Sizes, upload: Image.Upload)
  object Image {
    case class Sizes(mainview: Int, thumbnail: Int)
    object Sizes {
      implicit val json = Json.format[Sizes]
    }
    case class Upload(accessKey: String, secretKey: String, acl: String, timeout: FiniteDuration, minFileSize: Long, maxFileSize: Long)
    object Upload {
      implicit val json = Json.format[Upload]
    }
    implicit val json = Json.format[Image]
  }
  case class Facebook(host: String, appName: String, appId: String, publish: Facebook.Publish)
  object Facebook {
    case class Publish(imageTimeout: FiniteDuration, actionName: String, objectName: String)
    object Publish {
      implicit val json = Json.format[Publish]
    }
    implicit val json = Json.format[Facebook]
  }
  case class OpenWeatherMap(url: String, appId: String, iconUrl: String)
  object OpenWeatherMap {
    implicit val json = Json.format[OpenWeatherMap]
  }
  implicit val json = Json.format[Settings]
}

