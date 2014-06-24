import play.api._
import play.api.mvc._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._

object Global extends WithFilters(AccessLog)

object AccessLog extends Filter {
  override def apply(next: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] = {
    val result = next(request)
    result map { r =>
      Logger debug f"${request} headers: ${request.headers.toSimpleMap} \n\t =>  ${r}(status=${r.header.status}) headers: ${r.header.headers}"
    }
    result
  }
}
