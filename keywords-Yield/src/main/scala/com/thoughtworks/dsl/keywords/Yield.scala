package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  * @example This `Yield` keyword must be put inside a function that returns `Stream[Element]` or `Stream[Element] !! ...`,
  *          or it will not compile.
  *
  *          {{{
  *          "def f(): Unit = !Yield(1)" shouldNot compile
  *          }}}
  *
  */
final case class Yield[Element](element: Element) extends AnyVal with Keyword[Yield[Element], Unit]

object Yield {

  implicit def implicitYield[Element](element: Element): Yield[Element] = Yield[Element](element)

  implicit def yieldDsl[Element]: Dsl[Yield[Element], Stream[Element], Unit] =
    new Dsl[Yield[Element], Stream[Element], Unit] {
      def interpret(keyword: Yield[Element], mapper: Unit => Stream[Element]): Stream[Element] = {
        new Stream.Cons(keyword.element, mapper(()))
      }
    }
}
