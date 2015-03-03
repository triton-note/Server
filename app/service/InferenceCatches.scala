package service

import models.{ GeoInfo, Report }

object InferenceCatches {
  /**
   * Inference of Spot and Fishes.
   * This should finish before submission.
   */
  def infer(photo: Storage.S3File, geoinfo: Option[GeoInfo]): (String, Seq[Report.Fishes]) = {
    val location = ""
    val catches = Seq()
    (location, catches)
  }
}
