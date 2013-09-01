import play.api._
import play.api.mvc._
import play.api.Logger

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._

object Global extends WithFilters(AccessLog)

object AccessLog extends Filter {
  override def apply(next: RequestHeader => Result)(request: RequestHeader): Result = {
    val result = next(request)
    def headers(res: Result): Unit = res match {
      case r: PlainResult => play.Logger debug f"${request} cookies: ${request.cookies} \n\t =>  ${result} headers: ${r.header.headers}"
      case a: AsyncResult => a.result map headers
    }
    headers(result)
    result
  }
}
