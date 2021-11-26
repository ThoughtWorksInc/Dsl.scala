package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}

import scala.collection._
import scala.language.implicitConversions

import scala.collection.mutable.Builder

/** Iterates though each element in [[elements]]. */
final case class ForEach[Element](elements: Traversable[Element]) extends Keyword[ForEach[Element], Element]
object ForEach {

  implicit def implicitForEach[Element](elements: Traversable[Element]): ForEach[Element] = ForEach[Element](elements)

  implicit def foreachDsl[Element]: Dsl[ForEach[Element], Unit, Element] =
    new Dsl[ForEach[Element], Unit, Element] {
      def cpsApply(keyword: ForEach[Element], handler: Element => Unit): Unit = {
        keyword.elements.foreach(handler)
      }
    }

}
