package com.thoughtworks.each

import com.thoughtworks.each.annotations.shift

import scala.annotation.compileTimeOnly

/** Value dependent type type class that witnesses `Instruction` allows registering an asynchronous callback function to handle the `Value` when it complete.
  *
  * @tparam Value The value held inside `Instruction`.
  * @author 杨博 (Yang Bo)
  */
trait Continuation[-Instruction, State, +Value] {

  /** Registers an asynchronous callback function on `instruction`, to handle the `Value` when it complete */
  def cpsApply(instruction: Instruction, handler: Value => State): State

}

object Continuation {

  def apply[Instruction, State, Value](
      implicit typeClass: Continuation[Instruction, State, Value]): Continuation[Instruction, State, Value] = typeClass

  /**
    *
    * @tparam Instruction the self type
    * @see [[https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern Curiously recurring template pattern]] for the reason why we need `Instruction` type parameter
    */
  trait InstructionOps[Instruction, Value] extends Any {
    protected def self: Instruction // TODO: Use self type `this: Instruction =>` instead

    @shift
    @compileTimeOnly(
      """Magic call to `each` method requires compiler plugin: `addCompilerPlugin("com.thoughtworks.each" %% "compilerplugin" % "latest.release")`""")
    final def each: Value = ???

    /** Alias of `each` */
    @shift
    @compileTimeOnly(
      """Magic call to `!` method requires compiler plugin: `addCompilerPlugin("com.thoughtworks.each" %% "compilerplugin" % "latest.release")`""")
    final def unary_! : Value = ???

    //    def apply[State1, State0](handler: Value => State1)(implicit lift: Lift[State0, State], constraint: State1 <:< State0): State0 = {
    //      // FIXME: Use <:<.substitute instead of asInstanceOf for Scala 2.13
    //      val substitution = handler.asInstanceOf[Value => State0]
    //      lift.lift(underlying)(substitution)
    //    }
    final def cpsApply[State](handler: Value => State)(
        implicit continuation: Continuation[Instruction, State, Value]): State = {
      continuation.cpsApply(self, handler)
    }
  }
}
