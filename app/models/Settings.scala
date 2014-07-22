package models

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
}