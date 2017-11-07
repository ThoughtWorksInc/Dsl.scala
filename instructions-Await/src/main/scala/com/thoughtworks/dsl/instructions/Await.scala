package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

/**
  * @author 杨博 (Yang Bo)
  */
final case class Await[State, A](asyncFunction: (A => State) => State)
    extends AnyVal
    with Instruction[Await[State, A], A]

object Await {

  implicit def awaitCps[State, A]: Dsl[Await[State, A], State, A] =
    new Dsl[Await[State, A], State, A] {
      def interpret(self: Await[State, A], mapper: A => State): State = self.asyncFunction(mapper)
    }

}
