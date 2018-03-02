package com.thoughtworks.dsl

import scala.annotation._

/** The domain-specific interpreter for `Instruction` in `Domain`,
  * which is a dependent type type class that registers an asynchronous callback function,
  * to handle the `Value` inside `Instruction`.
  *
  * @tparam Value The value held inside `Instruction`.
  * @author 杨博 (Yang Bo)
  * @example Creating a collaborative DSL in [[https://github.com/ThoughtWorksInc/Dsl.scala Dsl.scala]] is easy.
  *          Only two steps are required:
  *
  *           - Defining their domain-specific [[com.thoughtworks.dsl.Dsl.Instruction Instruction]].
  *           - Implementing [[Dsl]] type class, which is a interpreter for an [[com.thoughtworks.dsl.Dsl.Instruction Instruction]].
  *
  *          TODO: an example for creating a DSL
  */
@implicitNotFound("Cannot interpret the DSL instruction ${Instruction} inside a function that returns ${Domain}.")
trait Dsl[-Instruction, Domain, +Value] {

  /** Registers an asynchronous callback `handler` on `instruction`, to handle the `Value`. */
  def interpret(instruction: Instruction, handler: Value => Domain): Domain

}

object Dsl {

  trait Trampoline1[A, R] extends Function1[A, R] {
    def step(): A => R

    @tailrec
    final def apply(a: A): R = {
      step() match {
        case trampoline: Trampoline1[A, R] =>
          trampoline(a)
        case last =>
          last(a)
      }
    }
  }

  implicit def continuationDsl[Instruction, Domain, FinalResult, InstructionValue](
      implicit restDsl: Dsl[Instruction, Domain, InstructionValue])
    : Dsl[Instruction, (FinalResult => Domain) => Domain, InstructionValue] = {
    new Dsl[Instruction, (FinalResult => Domain) => Domain, InstructionValue] {
      def interpret(
          instruction: Instruction,
          handler: InstructionValue => (FinalResult => Domain) => Domain): (FinalResult => Domain) => Domain = {
        (continue: FinalResult => Domain) =>
          restDsl.interpret(instruction, { a =>
            handler(a)(continue)
          })
      }
    }
  }

  private[dsl] /* sealed */ trait ResetAnnotation extends Annotation with StaticAnnotation
  private[dsl] final class nonTypeConstraintReset extends ResetAnnotation with StaticAnnotation

  /** An annotation to explicitly perform reset control operator on a code block. */
  final class reset extends ResetAnnotation with StaticAnnotation with TypeConstraint

  /** An annotation to mark a method is a shift control operator. */
  final class shift extends StaticAnnotation

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
//    @compileTimeOnly(
//      """This method requires the following compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugin" % "latest.release")`""")
    final def unary_! : Value = sys.error("Calls to this method should have been translated to `cpsApply`.")

    final def cpsApply[Domain](handler: Value => Domain)(implicit dsl: Dsl[Self, Domain, Value]): Domain = {
      dsl.interpret(this, handler)
    }

  }

}
