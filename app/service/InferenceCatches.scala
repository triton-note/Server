package service

import models.{GeoInfo, Record, Storage}

object InferenceCatches {
  /**
   * Inference of Date, Spot and Fishes.
   * This should finish before submission.
   */
  def infer(photo: Storage.S3File, geoinfo: Option[GeoInfo]): List[Record.Catches] = {
    Nil
  }
}
