import play.api._
import play.api.mvc._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._

object Global extends WithFilters(AccessLog)

object AccessLog extends Filter {
  override def apply(next: RequestHeader => Future[SimpleResult])(request: RequestHeader): Future[SimpleResult] = {
    val result = next(request)
    result map { r =>
      Logger debug f"${request} cookies: ${request.cookies} \n\t =>  ${r}(status=${r.header.status}) headers: ${r.header.headers}"
    }
    result
  }
}
