package com.thoughtworks.dsl
import org.scalatest.{AsyncTestSuite, BeforeAndAfterAll, Suite}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

/** @author 杨博 (Yang Bo)
  */
trait MockPingPongServer extends BeforeAndAfterAll { this: Suite =>

  implicit def executionContext: ExecutionContext

  protected implicit val system = akka.actor.ActorSystem()

  protected implicit val materializer = akka.stream.ActorMaterializer()

  protected val mockServer = {
    import akka.http.scaladsl.server.Directives._
    val route =
      get {
        path("ping") {
          complete("PING!")
        } ~ path("pong") {
          complete("PONG!")
        }
      }
    concurrent.Await.result(akka.http.scaladsl.Http().bindAndHandle(route, "localhost", 8085), Duration.Inf)
  }

  override protected def afterAll(): Unit = {
    mockServer
      .unbind()
      .onComplete(_ => system.terminate())
  }

}
