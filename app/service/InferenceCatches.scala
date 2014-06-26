package service

import models.{GeoInfo, Record, Storage}

object InferenceCatches {
  /**
   * Inference of Spot and Fishes.
   * This should finish before submission.
   */
  def infer(photo: Storage.S3File, geoinfo: Option[GeoInfo]): (String, Seq[Record.Fishes]) = {
    val catches = Seq(Record.Fishes("Snapper", 1))
    ("NeiSea", catches)
  }
}
