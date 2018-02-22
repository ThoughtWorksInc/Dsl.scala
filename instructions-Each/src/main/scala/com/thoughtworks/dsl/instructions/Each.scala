package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
final case class Each[Element](elements: Traversable[Element]) extends Instruction[Each[Element], Element]
object Each {
  def apply[Element](elements: Element*) = new Each[Element](elements)

  implicit def implicitEach[Element](elements: Traversable[Element]): Each[Element] = Each[Element](elements)

  implicit def eachDsl[Element, That, B](
      implicit thatIsTraversableOnce: (Element => That) <:< (Element => GenTraversableOnce[B]),
      bf: CanBuildFrom[Nothing, B, That]
  ): Dsl[Each[Element], That, Element] =
    new Dsl[Each[Element], That, Element] {
      def interpret(instruction: Each[Element], handler: Element => That): That = {
        instruction.elements.flatMap(handler)(collection.breakOut(bf))
      }
    }
}
