package service

import scala.concurrent._
import ExecutionContext.Implicits.global
import java.util.Date
import models._
import play.api.Logger

object InferencePreInfo {
  /**
   * Inference of Date, Spot and Fishes.
   * This should finish before submission.
   */
  def infer(info: Option[String]) = {
    info match {
      case Some(_) => info
      case None => {
        // TODO Inference by referencing all other CatchReports
      }
    }
  }
}
