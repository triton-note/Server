import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import play.api.{ Application, Logger }
import play.api.libs.concurrent.Akka
import play.api.mvc.{ Filter, RequestHeader, Result, WithFilters }

import models.db.VolatileToken

object Global extends WithFilters(AccessLog) {
  override def onStart(app: Application) {
    Akka.system(app).scheduler.schedule(0 seconds, 1 hours) {
      val count = VolatileToken.deleteExpired
      Logger debug f"Deleted ${count} rows on ${VolatileToken.tableName}"
    }
  }
}

object AccessLog extends Filter {
  override def apply(next: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] = {
    val result = next(request)
    result map { r =>
      Logger debug f"${request} headers: ${request.headers.toSimpleMap} \n\t =>  ${r}(status=${r.header.status}) headers: ${r.header.headers}"
    }
    result
  }
}
