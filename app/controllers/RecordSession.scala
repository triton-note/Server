package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.mvc.{Action, Controller}

object RecordSession extends Controller {

  def load(ticket: String) = Action.async { implicit request =>
    Future(NotImplemented)
  }
}