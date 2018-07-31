package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success}
import scala.concurrent.Await.result
import scala.concurrent.duration.Duration

final case class Await[Value](future: Future[Value]) extends AnyVal with Keyword[Await[Value], Value]

object Await {

  implicit def implicitAwait[Value](future: Future[Value]): Await[Value] = Await[Value](future)

  implicit def streamAwaitDsl[Value, That](
      implicit executionContext: ExecutionContext): Dsl[Await[Value], Stream[Future[That]], Value] =
    new Dsl[Await[Value], Stream[Future[That]], Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Stream[Future[That]]): Stream[Future[That]] = {
        import keyword.future
        val futureOfStream = future.map(handler)
        new Stream.Cons(futureOfStream.flatMap(_.head), result(futureOfStream, Duration.Inf).tail)
      }
    }

  implicit def awaitDsl[Value, That](
      implicit executionContext: ExecutionContext): Dsl[Await[Value], Future[That], Value] =
    new Dsl[Await[Value], Future[That], Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Future[That]): Future[That] = {
        keyword.future.flatMap(handler)
      }
    }

}
