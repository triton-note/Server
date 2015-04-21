import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

import play.api.{ Application, Logger }
import play.api.libs.concurrent.Akka
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{ Filter, RequestHeader, Result, WithFilters }

import models.VolatileToken

object Global extends WithFilters(AccessLog, CrossOriginResource) {
  override def onStart(app: Application) {
    Akka.system(app).scheduler.schedule(0 seconds, 1 hours) {
      val count = VolatileToken.deleteExpired
      Logger debug f"Deleted ${count} rows on ${VolatileToken.DB.TABLE.getTableName}"
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

object CrossOriginResource extends Filter {
  override def apply(next: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] = {
    val isPreFlight =
      request.method.toUpperCase.equals("OPTIONS") && request.headers.get("Access-Control-Request-Method").nonEmpty
    def addHeaders(r: Result) = r.withHeaders(
      "Access-Control-Allow-Origin" -> "*",
      "Access-Control-Allow-Methods" -> "POST, GET",
      "Access-Control-Allow-Headers" -> "Content-Type"
    )
    if (isPreFlight) Future { addHeaders(controllers.Default.Ok) }
    else next(request).map(addHeaders)
  }
}
