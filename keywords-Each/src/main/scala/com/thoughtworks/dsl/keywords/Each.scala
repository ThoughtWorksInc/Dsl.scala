package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Keyword

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
final case class Each[Element](elements: Traversable[Element]) extends Keyword[Each[Element], Element]
object Each {

  implicit def implicitEach[Element](elements: Traversable[Element]): Each[Element] = Each[Element](elements)

  implicit def eachDsl[Element, That, B](
      implicit thatIsTraversableOnce: (Element => That) <:< (Element => GenTraversableOnce[B]),
      bf: CanBuildFrom[Nothing, B, That]
  ): Dsl[Each[Element], That, Element] =
    new Dsl[Each[Element], That, Element] {
      def interpret(keyword: Each[Element], handler: Element => That): That = {
        keyword.elements.flatMap(handler)(collection.breakOut(bf))
      }
    }

  implicit def foreachDsl[Element]: Dsl[Each[Element], Unit, Element] =
    new Dsl[Each[Element], Unit, Element] {
      def interpret(keyword: Each[Element], handler: Element => Unit): Unit = {
        keyword.elements.foreach(handler)
      }
    }

}
