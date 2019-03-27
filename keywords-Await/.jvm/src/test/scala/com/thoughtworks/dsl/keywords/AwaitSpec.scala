package com.thoughtworks.dsl.keywords

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.thoughtworks.dsl.Dsl.reset
import org.scalatest.{AsyncFreeSpec, BeforeAndAfterAll, Matchers}
//import org.scalamock.scalatest.MockFactory

import scala.concurrent.Future
import scala.concurrent.duration._

/**
  * @author 杨博 (Yang Bo)
  */
final class AwaitSpec extends AsyncFreeSpec with Matchers with BeforeAndAfterAll with Directives {
  implicit val system = ActorSystem()

  implicit val materializer = ActorMaterializer()

  def downloadTwoPages(): Future[(ByteString, ByteString)] = Future {
    val response1 = !Await(Http().singleRequest(HttpRequest(HttpMethods.GET, PingUri)))
    val content1 = !Await(response1.entity.toStrict(timeout = 5.seconds))
    val response2 = !Await(Http().singleRequest(HttpRequest(HttpMethods.GET, PongUri)))
    val content2 = !Await(response2.entity.toStrict(timeout = 5.seconds))
    (content1.data, content2.data)
  }

  private val mockServer: Http.ServerBinding = {
    val route =
      get {
        path("ping") {
          complete("PING!")
        } ~ path("pong") {
          complete("PONG!")
        }
      }
    concurrent.Await.result(Http().bindAndHandle(route, "localhost", 0), Duration.Inf)
  }
  override protected def afterAll(): Unit = {
    mockServer
      .unbind()
      .onComplete(_ => system.terminate())
  }

  "download two pages" in {
    downloadTwoPages().map {
      case (bytes1, bytes2) =>
        bytes1.decodeString(io.Codec.UTF8.charSet) should be("PING!")
        bytes2.decodeString(io.Codec.UTF8.charSet) should be("PONG!")
    }
  }

  private val PingUri = Uri.from(scheme = "http",
                                 host = mockServer.localAddress.getHostName,
                                 port = mockServer.localAddress.getPort,
                                 path = "/ping")
  private val PongUri = Uri.from(scheme = "http",
                                 host = mockServer.localAddress.getHostName,
                                 port = mockServer.localAddress.getPort,
                                 path = "/pong")
  "http get" in ({
    val response = !Await(Http().singleRequest(HttpRequest(uri = PingUri)))
    response.status should be(StatusCodes.OK)
  }: @reset)

  "Downloading two web pages as an asynchronous generator, in the style of !-notation" in ({
    def downloadTwoPagesGenerator(): Stream[Future[ByteString]] = {
      val response1 = !Await(Http().singleRequest(HttpRequest(HttpMethods.GET, PingUri)))
      val content1 = !Await(response1.entity.toStrict(timeout = 5.seconds))
      !Yield(content1.data)
      val response2 = !Await(Http().singleRequest(HttpRequest(HttpMethods.GET, PongUri)))
      val content2 = !Await(response2.entity.toStrict(timeout = 5.seconds))
      !Yield(content2.data)

      Stream.empty[Future[ByteString]]
    }

    val stream = downloadTwoPagesGenerator()
    (!Await(stream(0))).decodeString(io.Codec.UTF8.charSet) should be("PING!")
    (!Await(stream(1))).decodeString(io.Codec.UTF8.charSet) should be("PONG!")
  }: @reset)

  "multiple http" in ({

    def createAsynchronousStream(): Stream[Future[Int]] = {
      val response1 = !Await(Http().singleRequest(HttpRequest(uri = PingUri)))
      !Yield(response1.status.intValue())
      response1.discardEntityBytes()
      val response2 = !Await(Http().singleRequest(HttpRequest(uri = PongUri)))
      !Yield(response2.status.intValue())
      response2.discardEntityBytes()
      Stream.empty[Future[Int]]
    }

    val asynchronousStream = createAsynchronousStream()
    !Await(asynchronousStream(0)) should be(StatusCodes.OK.intValue)
    !Await(asynchronousStream(1)) should be(StatusCodes.OK.intValue)

  }: @reset)

}
