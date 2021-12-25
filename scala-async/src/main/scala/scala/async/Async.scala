package scala.async
import scala.concurrent.Future
import com.thoughtworks.dsl.macros.Reset.Default.`*`
import com.thoughtworks.dsl.keywords.Await

/** Async blocks provide a direct means to work with
  * [[scala.concurrent.Future]].
  *
  * For example, to use an API that fetches a web page to fetch two pages and
  * add their lengths:
  *
  * {{{
  * import scala.concurrent._, duration._
  * import akka.util._, akka.actor._, akka.http.scaladsl._, model._
  * import scala.async.Async.{async, await}
  * implicit val system = ActorSystem()
  * import system.dispatcher
  * def fetchURL(url: Uri): Future[ByteString] = async {
  *   val response = await(Http().singleRequest(HttpRequest(HttpMethods.GET, url)))
  *   val entity = await(response.entity.toStrict(5.seconds))
  *   entity.data
  * }
  * }}}
  * @example
  * {{{
  * val sumLengths: Future[Int] = async {
  *   val body1 = fetchURL("https://scala-lang.org")
  *   val body2 = fetchURL("https://docs.scala-lang.org")
  *   await(body1).length + await(body2).length
  * }
  *
  * async {
  *   await(sumLengths) should be > 50000
  * }
  * }}}
  * @example
  *   Note that in the following program, the second fetch does *not* start
  *   until after the first. If you need to start tasks in parallel, you must do
  *   so before `await`-ing a result.
  *
  * {{{
  * val sumLengths: Future[Int] = async {
  *   await(fetchURL("https://scala-lang.org")).length + await(fetchURL("https://docs.scala-lang.org")).length
  * }
  *
  * async {
  *   await(sumLengths) should be > 50000
  * }
  * }}}
  */
object Async {
  def async = *[Future]
  inline def await[T](inline awaitable: Future[T]): T = !Await(awaitable)
}
