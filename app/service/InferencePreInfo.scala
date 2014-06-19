package service

import scala.concurrent._
import ExecutionContext.Implicits.global
import java.util.Date
import models._
import play.api.Logger
import controllers.RecordSession

object InferencePreInfo {
  /**
   * Inference of Date, Spot and Fishes.
   * This should finish before submission.
   */
  def infer(photo: Storage.S3File, geoinfo: Option[GeoInfo]): List[RecordSession.FishInfo] = {
    Nil
  }
}
