package service

import models.{ GeoInfo, Report, Storage }

object InferenceCatches {
  /**
   * Inference of Spot and Fishes.
   * This should finish before submission.
   */
  def infer(photo: Storage.S3File, geoinfo: Option[GeoInfo]): (String, Seq[Report.Fishes]) = {
    val catches = Seq(Report.Fishes("Snapper", 1))
    ("NeiSea", catches)
  }
}
