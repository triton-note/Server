package models.db

import java.util.Date

import scala.collection.JavaConversions._
import scala.concurrent.duration._

import com.amazonaws.services.dynamodbv2.model._

case class VolatileToken(MAP: Map[String, AttributeValue]) extends TimestampedTable.ObjType[VolatileToken] {
  val TABLE = VolatileToken

  lazy val expiration: Date = build(_.expiration)
  lazy val extra: Option[String] = build(_.extra)
  /**
   * Change properties (like a copy) and update Database
   */
  def update(extra: Option[String] = this.extra, expiration: Date = this.expiration): Option[VolatileToken] = {
    TABLE.update(id, Map(
      TABLE.extra(extra),
      TABLE.expiration(expiration)
    ))(for {
      (n, v) <- Map(TABLE.extra(this.extra))
    } yield n -> new ExpectedAttributeValue(v).withComparisonOperator(ComparisonOperator.EQ))
  }
  def setExtra(text: String) = update(extra = Some(text))
  def removeExtra = update(extra = None)
  def changeExpiration(theNext: Date) = update(expiration = theNext)
}
object VolatileToken extends AutoIDTable[VolatileToken]("VOLATILE_TOKEN") {
  val expiration = Column[Date]("EXPIRATION", (_.expiration), (_.getDate.get), attrDate)
  val extra = Column[Option[String]]("EXTRA", (_.extra), (_.getString), attrString)
  // All columns
  val columns = List(expiration, extra)
  /**
   * Add new token
   */
  def addNew(willExpired: FiniteDuration, theExtra: Option[String] = None): VolatileToken = addNew(
    expiration(new Date(currentTimestamp.getTime + willExpired.toMillis)),
    extra(theExtra)
  )
  /**
   * Delete expired tokens
   * @return number of deleted
   */
  def deleteExpired: Int = {
    val expired = scan(
      _.withAttributesToGet(id.name).withScanFilter(Map(
        expiration.compare(currentTimestamp, ComparisonOperator.LE)
      )), id.build)
    expired.par.map(delete).filter(_ == true).size
  }
}
