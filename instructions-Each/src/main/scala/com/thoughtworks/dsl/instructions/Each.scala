package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom

/**
  * @author 杨博 (Yang Bo)
  */
final case class Each[Element](elements: Element*) extends Instruction[Each[Element], Element]
object Each {
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
