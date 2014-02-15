package models

import java.io.File
import scala.concurrent.duration._

object Settings {
  object Session {
    def timeoutFacebook: FiniteDuration = 30 minutes
    def timeoutUpload: FiniteDuration = 1 hour
  }
  object Pulish {
    def timer = 3 minutes
  }
}