package com.thoughtworks.dsl.keywords

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.stream.ActorMaterializer
import com.thoughtworks.dsl.Dsl.reset
import org.scalatest.{AsyncFreeSpec, Matchers}

import scala.concurrent.Future

/**
  * @author 杨博 (Yang Bo)
  */
final class AwaitSpec extends AsyncFreeSpec with Matchers {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()

  "https get" in ({
    val response = !Await(Http().singleRequest(HttpRequest(uri = "http://example.com")))
    response.status should be(StatusCodes.OK)
  }: @reset)

  "multiple https" in ({

    def createAsynchronousStream(): Stream[Future[Int]] = {
      val response1 = !Await(Http().singleRequest(HttpRequest(uri = "http://example.com")))
      !Yield(response1.status.intValue())
      response1.discardEntityBytes()
      val response2 = !Await(Http().singleRequest(HttpRequest(uri = "http://example.net")))
      !Yield(response2.status.intValue())
      response2.discardEntityBytes()
      Stream.empty[Future[Int]]
    }

    val asynchronousStream = createAsynchronousStream()
    !Await(asynchronousStream(0)) should be(StatusCodes.OK.intValue)
    !Await(asynchronousStream(1)) should be(StatusCodes.OK.intValue)

  }: @reset)

}
