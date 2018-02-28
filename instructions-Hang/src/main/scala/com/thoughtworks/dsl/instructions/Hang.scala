package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

/**
  * @author 杨博 (Yang Bo)
  */
sealed case class Hang[Value]() extends Instruction[Hang[Value], Value]

object Hang {

  implicit def hangDsl[Value]: Dsl[Hang[Value], Unit, Value] = new Dsl[Hang[Value], Unit, Value] {
    def interpret(instruction: Hang[Value], handler: Value => Unit): Unit = ()
  }

}
