package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success}

final case class Await[Value](future: Future[Value]) extends AnyVal with Keyword[Await[Value], Value]

object Await {

  implicit def implicitAwait[Value](future: Future[Value]): Await[Value] = Await[Value](future)

  private def asynchronousLazyCons[Value, That](future: Future[Value])(handler: Value => Stream[Future[That]])(
      implicit executionContext: ExecutionContext): Stream[Future[That]] = {
    future.value match {
      case Some(Success(value)) =>
        handler(value)
      case Some(Failure(e)) =>
        Future.failed[That](e) #:: Stream.empty[Future[That]]
      case None =>
        val futureOfStream = future.map(handler)
        futureOfStream.flatMap(_.head) #:: asynchronousLazyCons(futureOfStream)(_.tail)
    }
  }

  implicit def streamAwaitDsl[Value, That](
      implicit executionContext: ExecutionContext): Dsl[Await[Value], Stream[Future[That]], Value] =
    new Dsl[Await[Value], Stream[Future[That]], Value] {
      def cpsApply(keyword: Await[Value], handler: Value => Stream[Future[That]]): Stream[Future[That]] = {
        asynchronousLazyCons(keyword.future)(handler)
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
