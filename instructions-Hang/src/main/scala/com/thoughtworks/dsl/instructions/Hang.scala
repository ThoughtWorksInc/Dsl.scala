package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
sealed case class Hang[Value]() extends Instruction[Hang[Value], Value]

object Hang {

  implicit def hangUnitDsl[Value]: Dsl[Hang[Value], Unit, Value] = new Dsl[Hang[Value], Unit, Value] {
    def interpret(instruction: Hang[Value], handler: Value => Unit): Unit = ()
  }

  implicit def hangStreamDsl[Value, Element]: Dsl[Hang[Value], Stream[Element], Value] =
    new Dsl[Hang[Value], Stream[Element], Value] {
      def interpret(instruction: Hang[Value], handler: Value => Stream[Element]): Stream[Element] = Stream.empty
    }

}
