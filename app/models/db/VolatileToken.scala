package models.db

import java.util.Date

import scala.annotation.tailrec
import scala.collection.JavaConversions._
import scala.concurrent.duration._

import scalaz.Scalaz._

import play.api.Logger

import org.fathens.play.util.Exception.allCatch

import com.amazonaws.services.dynamodbv2.model._

case class VolatileToken(id: String,
  createdAt: Date,
  lastModifiedAt: Option[Date],
  expiration: Date,
  extra: Option[String]) extends TimestampedTable.ObjType[VolatileToken] {
  val TABLE = VolatileToken
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
  def fromMap(implicit map: Map[String, AttributeValue]): Option[VolatileToken] = allCatch opt VolatileToken(
    id.build,
    createdAt.build,
    lastModifiedAt.build,
    expiration.build,
    extra.build
  )
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
      )), map => Some(id build map))
    expired.par.toList.map(delete).filter(_ == true).size
  }
}
