package models

case class GeoInfo(latitude: Double, longitude: Double)
object GeoInfo {
  def apply(a: Option[Double], o: Option[Double]): Option[GeoInfo] = for {
    la <- a
    lo <- o
  } yield apply(la, lo)
}