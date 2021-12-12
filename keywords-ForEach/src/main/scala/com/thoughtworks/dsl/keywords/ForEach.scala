package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.AsKeyword

import scala.collection._
import scala.language.implicitConversions

import scala.collection.mutable.Builder

/** Iterates though each element in [[elements]]. */
final case class ForEach[Element](elements: Traversable[Element]) extends Dsl.Keyword.Trait
object ForEach {
  given [Element]: AsKeyword.IsKeyword[ForEach[Element], Element] with {}
  given [Element]: AsKeyword[Traversable[Element], ForEach[Element], Element] = ForEach(_)

  implicit def foreachDsl[Element]: Dsl[ForEach[Element], Unit, Element] =
    new Dsl[ForEach[Element], Unit, Element] {
      def cpsApply(keyword: ForEach[Element], handler: Element => Unit): Unit = {
        keyword.elements.foreach(handler)
      }
    }

}
