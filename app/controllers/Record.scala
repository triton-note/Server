package controllers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import play.api.mvc.{Action, Controller}

object Record extends Controller {

  def load = Action.async { implicit request =>
    Future(NotImplemented)
  }
}