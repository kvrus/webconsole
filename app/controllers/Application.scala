package controllers

import models._
import play.api._
import play.api.mvc._
import play.api.libs.json._

//https://www.playframework.com/documentation/1.1/api/play/mvc/Http.Response.html
object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def test(cmd: String) = Action {
  	//Ok("Got result: " + Parser.parse(2,cmd))
  	val list = Parser.parse(2,cmd);
    //http://stackoverflow.com/questions/1094173/how-do-i-get-around-type-erasure-on-scala-or-why-cant-i-get-the-type-paramete
  	/*
    list match {
        case l:List[Account @unchecked] => Ok(views.html.block(l));
        case _ => Ok("nothing");
    }
    */
    Ok("nothing");
  }
  
  def feed(url: String) = Action {
    Async {
      play.api.libs.ws.WS.url(url).get().map { response =>
        Ok(response.json)
      }
    }  
  }

}