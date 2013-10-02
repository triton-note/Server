import play.api._
import play.api.mvc._
import play.api.Logger

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._

object Global extends WithFilters(AccessLog)

object AccessLog extends Filter {
  override def apply(next: RequestHeader => Future[SimpleResult])(request: RequestHeader): Future[SimpleResult] = {
    val result = next(request)
    result map { r =>
      play.Logger debug f"${request} cookies: ${request.cookies} \n\t =>  ${result} headers: ${r.header.headers}"
    }
    result
  }
}
