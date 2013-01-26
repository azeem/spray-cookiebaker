package com.example

import akka.actor.Actor
import sessionutils.{SessionDirectives, Session}
import spray.routing._
import spray.http._
import MediaTypes._
import com.typesafe.config.ConfigFactory
import com.example.sessionutils.SessionDirectives._


// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class MyServiceActor extends Actor with MyService {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(myRoute)
}


// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService {
  val myRoute =
    (path("clear") & get & respondWithMediaType(`text/html`) & clearSession) {
      complete {
        <html>
          <body>
            <h1>The session has been Cleared</h1>
          </body>
        </html>
      }
    } ~
    (path("set") & get & respondWithMediaType(`text/html`) & setSession("name" -> "hello")) {
      complete {
        <html>
          <body>
            <h1>The session has been Set</h1>
          </body>
        </html>
      }
    } ~
    (path("get") & get & respondWithMediaType(`text/html`)) {
      SessionDirectives.session { sessionData:Session =>
        complete {
          <html>
            <body>
              <h1>Session value: {sessionData.data.toString}</h1>
            </body>
          </html>
        }
      }
    } ~
    (path("getoptional") & get & respondWithMediaType(`text/html`)) {
      optionalSession { sessionData:Option[Session] =>
        complete {
          <html>
            <body>
              {
                sessionData.map { sess =>
                  <h1>Session value: {sess.data.toString}</h1>
                } getOrElse {
                  <h1>Session not set</h1>
                }
              }
            </body>
          </html>
        }
      }
    }

}