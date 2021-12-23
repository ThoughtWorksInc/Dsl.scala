package com.thoughtworks.dsl
package domains

import scala.util._
import scala.util.control.NonFatal
import com.thoughtworks.dsl.keywords.Pure

type Continuation[R, +A] = (A => R) => R

object Continuation {
  val !! = this
  type !![R, +A] = Continuation[R, A]

  @inline
  def now[R, A](a: A): R !! A = _(a)

  @inline
  def empty[R, A](r: R): R !! A = Function.const(r)

  @inline
  def delay[R, A](a: () => A): R !! A = _(a())

  inline def apply[R, A](inline a: A): R !! A = { handler =>
    reset {
      // !Pure ensures stack safety
      handler(!Pure(a))
    }
  }

  def toTryContinuation[LeftDomain, Value](
      task: LeftDomain !! Throwable !! Value
  )(handler: Try[Value] => LeftDomain): LeftDomain = {
    task { a => failureHandler =>
      handler(Success(a))
    } { e =>
      handler(Failure(e))
    }
  }

  def fromTryContinuation[LeftDomain, Value](
      continuation: LeftDomain !! Try[Value]
  )(
      successHandler: Value => LeftDomain !! Throwable
  )(failureHandler: Throwable => LeftDomain): LeftDomain = {
    continuation(
      new (Try[Value] => LeftDomain) {
        def apply(result: Try[Value]): LeftDomain = {
          result match {
            case Success(a) =>
              val protectedContinuation =
                try {
                  successHandler(a)
                } catch {
                  case NonFatal(e) =>
                    return failureHandler(e)
                }
              protectedContinuation(failureHandler)
            case Failure(e) =>
              failureHandler(e)
          }
        }
      }
    )
  }

}
