package org.fathens.play.util

import play.api.Logger

object Exception {
  object allCatch {
    def opt[A](p: => A): Option[A] = {
      try {
        Some(p)
      } catch {
        case ex: Throwable =>
          Logger.error(ex.getMessage, ex)
          None
      }
    }
  }
}