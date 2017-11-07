package com.thoughtworks.dsl

import com.thoughtworks.dsl.delimitedcontinuation.annotations.shift

import scala.annotation.compileTimeOnly

/** The domain-specific interpreter for `Instruction`,
  * which is a dependent type type class that registers an asynchronous callback function on `Instruction`,
  * to handle the `Value` when it complete.
  *
  * @tparam Value The value held inside `Instruction`.
  * @author 杨博 (Yang Bo)
  */
trait Dsl[-Instruction, State, +Value] {

  /** Registers an asynchronous callback function on `instruction`, to handle the `Value` when it complete */
  def interpret(instruction: Instruction, handler: Value => State): State

}

object Dsl {

  def apply[Instruction, State, Value](
      implicit typeClass: Dsl[Instruction, State, Value]): Dsl[Instruction, State, Value] =
    typeClass

  /**
    *
    * @tparam Self the self type
    * @see [[https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern Curiously recurring template pattern]] for the reason why we need `Instruction` type parameter
    */
  trait Instruction[Self, Value] extends Any { this: Self =>

    @shift
    @compileTimeOnly(
      """Magic call to `!` method requires compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "delimitedcontinuation-compilerplugin" % "latest.release")`""")
    final def unary_! : Value = ???

    //    def cpsApply[State1, State0](handler: Value => State1)(implicit lift: Lift[State0, State], constraint: State1 <:< State0): State0 = {
    //      // FIXME: Use <:<.substitute instead of asInstanceOf for Scala 2.13
    //      val substitution = handler.asInstanceOf[Value => State0]
    //      lift.lift(underlying)(substitution)
    //    }
    final def cpsApply[State](handler: Value => State)(implicit dsl: Dsl[Self, State, Value]): State = {
      dsl.interpret(this, handler)
    }

  }

}
