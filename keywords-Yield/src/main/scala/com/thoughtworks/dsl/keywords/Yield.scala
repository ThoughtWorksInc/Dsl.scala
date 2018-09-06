package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword

import scala.concurrent.Future
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  * @example This `Yield` keyword must be put inside a function that returns `Stream[Element]` or `Stream[Element] !! ...`,
  *          or it will not compile.
  *
  *          {{{
  *          "def f(): Int = !Yield(1)" shouldNot compile
  *          }}}
  *
  */
final case class Yield[Element](element: Element) extends AnyVal with Keyword[Yield[Element], Unit]

object Yield {

  final case class From[Element](elements: Stream[Element]) extends AnyVal with Keyword[From[Element], Unit]

  def apply[Element](element0: Element, element1: Element, restElements: Element*) = {
    From(new Stream.Cons(element0, new Stream.Cons(element1, restElements.toStream)))
  }

  implicit def implicitYieldFrom[Element](elements: Stream[Element]): From[Element] = From[Element](elements)

  implicit def yieldFromDsl[Element, That >: Element]: Dsl[From[Element], Stream[That], Unit] =
    new Dsl[From[Element], Stream[That], Unit] {
      def cpsApply(keyword: From[Element], generateTail: Unit => Stream[That]): Stream[That] = {
        keyword.elements.append(generateTail(()))
      }
    }

  implicit def futureYieldFromDsl[Element, That >: Element]: Dsl[From[Element], Stream[Future[That]], Unit] =
    new Dsl[From[Element], Stream[Future[That]], Unit] {
      def cpsApply(keyword: From[Element], generateTail: Unit => Stream[Future[That]]): Stream[Future[That]] = {
        keyword.elements.map(Future.successful).append(generateTail(()))
      }
    }

  implicit def implicitYield[Element](element: Element): Yield[Element] = Yield[Element](element)

  implicit def yieldDsl[Element, That >: Element]: Dsl[Yield[Element], Stream[That], Unit] =
    new Dsl[Yield[Element], Stream[That], Unit] {
      def cpsApply(keyword: Yield[Element], generateTail: Unit => Stream[That]): Stream[That] = {
        new Stream.Cons(keyword.element, generateTail(()))
      }
    }

  implicit def futureYieldDsl[Element, That >: Element]: Dsl[Yield[Element], Stream[Future[That]], Unit] =
    new Dsl[Yield[Element], Stream[Future[That]], Unit] {
      def cpsApply(keyword: Yield[Element], generateTail: Unit => Stream[Future[That]]): Stream[Future[That]] = {
        new Stream.Cons(Future.successful(keyword.element), generateTail(()))
      }
    }
}
