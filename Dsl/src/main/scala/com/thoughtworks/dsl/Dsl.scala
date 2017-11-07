package com.thoughtworks.dsl

import com.thoughtworks.dsl.delimitedcontinuation.annotations.shift

import scala.annotation.compileTimeOnly

/** The domain-specific interpreter for `Instruction` in `Domain`,
  * which is a dependent type type class that registers an asynchronous callback function,
  * to handle the `Value` inside `Instruction`.
  *
  * @tparam Value The value held inside `Instruction`.
  * @author 杨博 (Yang Bo)
  */
trait Dsl[-Instruction, Domain, +Value] {

  /** Registers an asynchronous callback `handler` on `instruction`, to handle the `Value`. */
  def interpret(instruction: Instruction, handler: Value => Domain): Domain

}

object Dsl {

  def apply[Instruction, Domain, Value](
      implicit typeClass: Dsl[Instruction, Domain, Value]): Dsl[Instruction, Domain, Value] =
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

    //    def cpsApply[Domain1, Domain0](handler: Value => Domain1)(implicit lift: Lift[Domain0, Domain], constraint: Domain1 <:< Domain0): Domain0 = {
    //      // FIXME: Use <:<.substitute instead of asInstanceOf for Scala 2.13
    //      val substitution = handler.asInstanceOf[Value => Domain0]
    //      lift.lift(underlying)(substitution)
    //    }
    final def cpsApply[Domain](handler: Value => Domain)(implicit dsl: Dsl[Self, Domain, Value]): Domain = {
      dsl.interpret(this, handler)
    }

  }

}
