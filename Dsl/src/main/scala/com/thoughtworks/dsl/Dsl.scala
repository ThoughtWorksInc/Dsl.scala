package com.thoughtworks.dsl

import scala.annotation.{Annotation, StaticAnnotation, TypeConstraint, compileTimeOnly}

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

  implicit def scopeDsl[Instruction, Domain, FinalResult, InstructionValue](
      implicit restDsl: Dsl[Instruction, Domain, InstructionValue])
    : Dsl[Instruction, (FinalResult => Domain) => Domain, InstructionValue] = {
    new Dsl[Instruction, (FinalResult => Domain) => Domain, InstructionValue] {
      def interpret(instruction: Instruction,
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
