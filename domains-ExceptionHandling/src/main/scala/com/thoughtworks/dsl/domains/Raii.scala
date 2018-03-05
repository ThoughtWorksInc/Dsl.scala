package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Continuation
import com.thoughtworks.dsl.domains.Raii.{ExitScope, Throwing}
import com.thoughtworks.dsl.instructions.Shift.StackSafeShiftDsl
import com.thoughtworks.dsl.instructions.{Catch, Scope, Shift}

import scala.util.{Success, Try}
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
trait Raii[Domain] extends Continuation[Domain, Raii[Domain]] {
  def onFailure(failureHandler: Throwable => Domain): Domain = {
    this(new (Raii[Domain] => Domain) {
      def apply(raii: Raii[Domain]): Domain = {
        raii match {
          case Throwing(e) =>
            failureHandler(e)
          case exitScope: ExitScope[Domain] =>
            throw new IllegalStateException("unmatched scope")
          case _ =>
            raii(this)
        }
      }
    })
  }
}

object Raii {

  implicit final class RaiiContinuationOps[Domain, A](task: Continuation[Raii[Domain], A]) {
    def onComplete(handler: Try[A] => Domain): Domain = {
      catchJvmException(
        task { a =>
          new Hang[Domain] {
            def onSuccess(): Domain = {
              handler(Success(a))
            }
          }
        },
        new (Raii[Domain] => Domain) {
          def apply(raii: Raii[Domain]): Domain = {
            raii match {
              case Throwing(e) =>
                handler(scala.util.Failure(e))
              case exitScope: ExitScope[Domain] =>
                throw new IllegalStateException("unmatched scope")
              case _ =>
                raii(this)
            }
          }
        }
      )
    }
  }

  trait ExitScope[Domain] extends Raii[Domain] {
    override def toString(): String = "ExitScope"
  }

  trait Hang[Domain] extends Raii[Domain] {
    def onSuccess(): Domain

    def apply(continue: Raii[Domain] => Domain): Domain = {
      onSuccess()
    }

    override def toString(): String = "Hang"

  }

  final case class Throwing[Domain](throwable: Throwable) extends ExitScope[Domain] {
    override def apply(continue: Raii[Domain] => Domain): Domain = {
      continue(this)
    }

    override def toString(): String = "Throwing"
  }

  def success[Domain](domain: Domain): Raii[Domain] = new Hang[Domain] {
    def onSuccess(): Domain = domain
  }

  def failure[Domain](throwable: Throwable): Raii[Domain] = Throwing[Domain](throwable)

  implicit def scopeDsl[Domain, A](implicit shiftDsl: Dsl[Shift[Domain, Raii[Domain]], Domain, Raii[Domain]])
    : Dsl[Scope[Raii[Domain], A], Raii[Domain], A] =
    new Dsl[Scope[Raii[Domain], A], Raii[Domain], A] {
      def interpret(scope: Scope[Raii[Domain], A], handler: A => Raii[Domain]): Raii[Domain] = {
        new Raii[Domain] {
          def apply(outerScope: Raii[Domain] => Domain): Domain = {
            val runScope = new (Raii[Domain] => Domain) {
              def apply(raii: Raii[Domain]) = raii match {
                case exitScope: ExitScope[Domain] =>
                  exitScope(outerScope)
                case raii =>
                  shiftDsl.interpret(Shift(raii), this)
              }
            }

            runScope(scope.continuation { a =>
              new ExitScope[Domain] {
                def apply(endScope: Raii[Domain] => Domain): Domain = {
                  catchJvmException(handler(a), endScope)
                }
              }
            })

          }
        }

      }
    }

  implicit def catchDsl[Domain](implicit shiftDsl: Dsl[Shift[Domain, Raii[Domain]], Domain, Raii[Domain]])
    : Dsl[Catch[Raii[Domain]], Raii[Domain], Unit] = {
    new Dsl[Catch[Raii[Domain]], Raii[Domain], Unit] {
      def interpret(instruction: Catch[Raii[Domain]], body: Unit => Raii[Domain]): Raii[Domain] = {
        val Catch(catcher) = instruction
        new Raii[Domain] {
          def apply(continue: Raii[Domain] => Domain): Domain = {
            val runScope = new (Raii[Domain] => Domain) {
              def apply(raii: Raii[Domain]) = raii match {
                case Throwing(e) =>
                  continue(catcher(e))
                case exitScope: ExitScope[Domain] =>
                  continue(exitScope)
                case raii =>
                  shiftDsl.interpret(Shift(raii), this)
              }
            }
            val raii = try {
              body(())
            } catch {
              case NonFatal(e) =>
                return continue(catcher(e))
            }
            runScope(raii)
          }
        }
      }
    }
  }

  implicit def liftRaiiDsl[Instruction, OtherDomain, A](
      implicit restDsl: Dsl[Instruction, OtherDomain, A]
  ): Dsl[Instruction, Raii[OtherDomain], A] =
    new Dsl[Instruction, Raii[OtherDomain], A] {
      def interpret(instruction: Instruction, successHandler: A => Raii[OtherDomain]): Raii[OtherDomain] =
        new Raii[OtherDomain] {
          def apply(continue: Raii[OtherDomain] => OtherDomain): OtherDomain = {
            restDsl.interpret(instruction, { a =>
              continue(try {
                successHandler(a)
              } catch {
                case NonFatal(e) =>
                  Throwing(e)
              })
            })
          }
        }

    }

  @inline def catchJvmException[OtherDomain](eh: => Raii[OtherDomain],
                                             failureHandler: Raii[OtherDomain] => OtherDomain): OtherDomain = {
    val protectedRaii: Raii[OtherDomain] = try {
      eh
    } catch {
      case NonFatal(e) =>
        return failureHandler(Throwing(e))
    }
    protectedRaii.apply(failureHandler)
  }

}
