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
  object Storage {
    def retryLimit: Int = 5
  }
}