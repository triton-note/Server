package service

import scala.concurrent.duration._

object Settings {
  object Session {
    def timeoutTicket: FiniteDuration = 30 minutes
    def timeoutUpload: FiniteDuration = 1 hour
  }
  object Pulish {
    def timer: FiniteDuration = 3 minutes
  }
  object Image {
    def sizeMainview = 800
    def sizeThumbnail = 200
    def uploadExpiration = 5 minutes
    def uploadMaxSize = 10 * 1024 * 1024
    def urlExpiration = 1 hour
  }

  def get(name: String): String = System.getProperty(name, System.getenv(name))

  lazy val AWS_REGION = get("AWS_REGION")
  lazy val AWS_ACCESS_KEY_ID = get("AWS_ACCESS_KEY_ID")
  lazy val AWS_SECRET_ACCESS_KEY = get("AWS_SECRET_ACCESS_KEY")
  lazy val AWS_S3_BUCKET_NAME = get("AWS_S3_BUCKET_NAME")
  lazy val AWS_ACCESS_KEY_ID_CLIENTSIDE = get("AWS_ACCESS_KEY_ID_CLIENTSIDE")
  lazy val AWS_SECRET_ACCESS_KEY_CLIENTSIDE = get("AWS_SECRET_ACCESS_KEY_CLIENTSIDE")

  lazy val FACEBOOK_HOST = get("FACEBOOK_HOST")
  lazy val FACEBOOK_APP_NAME = get("FACEBOOK_APP_NAME")
  lazy val FACEBOOK_CATCH_ACTION = get("FACEBOOK_CATCH_ACTION")
  lazy val FACEBOOK_CATCH_OBJECT = get("FACEBOOK_CATCH_OBJECT")
  lazy val FACEBOOK_APP_ID = get("FACEBOOK_APP_ID")

  lazy val GOOGLEPLUS_APPLICATION_NAME = get("GOOGLEPLUS_APPLICATION_NAME")
  lazy val GOOGLEPLUS_API_KEY = get("GOOGLEPLUS_API_KEY")

  lazy val OPENWEATHERMAP_URL = get("OPENWEATHERMAP_URL")
  lazy val OPENWEATHERMAP_APPID = get("OPENWEATHERMAP_APPID")
  lazy val OPENWEATHERMAP_ICON_URL = get("OPENWEATHERMAP_ICON_URL")
}
