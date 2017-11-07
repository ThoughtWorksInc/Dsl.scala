package com.thoughtworks.dsl.states

import com.thoughtworks.dsl.Dsl

import scala.util.control.Exception.Catcher
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
trait MayFail[State] { self =>
  def onFailure(handler: Throwable => State): State

  final def cpsCatch(catcher: Catcher[MayFail[State]]): MayFail[State] = new MayFail[State] {
    def onFailure(failureHandler: Throwable => State): State = {
      self.onFailure { e =>
        object Extractor {
          def unapply(e: Throwable): Option[MayFail[State]] = catcher.lift(e)
        }

        e match {
          case Extractor(handled) => handled.onFailure(failureHandler)
          case _                  => failureHandler(e)
        }

      }
    }
  }

}

object MayFail {
  def success[State](r: State): MayFail[State] = new MayFail[State] {
    def onFailure(handler: Throwable => State): State = r
  }

  def failure[State](e: Throwable): MayFail[State] = new MayFail[State] {
    def onFailure(handler: Throwable => State): State = handler(e)
  }

  implicit def mayFailCpsApply[Instruction, State, A](
      implicit restCps: Dsl[Instruction, State, A]): Dsl[Instruction, MayFail[State], A] =
    new Dsl[Instruction, MayFail[State], A] {
      def interpret(continuation: Instruction, successHandler: A => MayFail[State]): MayFail[State] =
        new MayFail[State] {
          def onFailure(failureHandler: Throwable => State): State = {
            def restHandler(a: A): State =
              (try {
                successHandler(a)
              } catch {
                case NonFatal(e) =>
                  return failureHandler(e)
              }).onFailure(failureHandler)

            restCps.interpret(continuation, restHandler)
          }
        }

    }

}
