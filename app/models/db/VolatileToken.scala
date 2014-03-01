package models.db

import java.util.Date
import scala.util.control.Exception._
import scalaz._
import Scalaz._
import com.amazonaws.services.dynamodbv2.model._
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration

case class VolatileToken(id: String,
                         createdAt: Date,
                         lastModifiedAt: Option[Date],
                         expiration: Date,
                         extra: Option[String]) {
  /**
   * Reload form DB
   */
  def refresh: Option[VolatileToken] = VolatileTokens.get(id)
  /**
   * Delete me
   */
  def delete: Boolean = VolatileTokens.delete(id)
  /**
   * Change properties (like a copy) and update Database
   */
  def update(extra: Option[String] = this.extra, expiration: Date = this.expiration): Option[VolatileToken] = VolatileTokens.update(id,
    VolatileTokens.extra(extra),
    VolatileTokens.expiration(expiration)
  )
  def setExtra(xml: scala.xml.Elem) = update(extra = Some(xml.toString))
  def removeExtra = update(extra = None)
  def changeExpiration(theNext: Date) = update(expiration = theNext)
}
object VolatileTokens extends AnyIDTable[VolatileToken]("VOLATILE_TOKEN") {
  val expiration = Column[Date]("EXPIRATION", (_.expiration), (_.getDate), attrDate)
  val extra = Column[Option[String]]("EXTRA", (_.extra), (_.getS.some), attrString)
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
  def addNew(theToken: String, willExpired: FiniteDuration, theExtra: Option[String] = None): Option[VolatileToken] = addNew(theToken,
    expiration(new Date(currentTimestamp.getTime + willExpired.toMillis)),
    extra(theExtra)
  )
  /**
   * Create new token and save to database
   */
  @tailrec
  def createNew(willExpired: FiniteDuration, extra: Option[String] = None): VolatileToken = {
    val token = play.api.libs.Codecs.sha1(System.currentTimeMillis.toString)
    addNew(token, willExpired, extra) match {
      case None    => createNew(willExpired, extra)
      case Some(a) => a
    }
  }
  /**
   * Delete expired tokens
   * @return number of deleted
   */
  def deleteExpired: Int = {
    import scala.collection.JavaConversions._
    val q = new QueryRequest(tableName).withKeyConditions(Map(
      expiration.name -> new Condition().withComparisonOperator(ComparisonOperator.LE).withAttributeValueList(expiration(currentTimestamp)._2)
    ))
    val expired = for {
      result <- Option(service.AWS.DynamoDB.client.query(q)).toSet
      item <- result.getItems
      o <- fromMap(item.toMap)
    } yield o
    expired.map(_.delete).filter(_ == true).size
  }
}
