package com.thoughtworks.each

import org.scalatest.{FreeSpec, Matchers}

import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
class EachOpsSpec extends FreeSpec with Matchers {
  import EachOpsSpec._
  "Given a naive EachOps" - {

    "Given a generator that contains conditional Yield" - {
      def generator = {
        if (false) {
          !Yield(0)
        }
        if (true) {
          !Yield(1)
        }
        if ({ !Yield(2); false }) {
          !Yield(3)
        } else {
          !Yield(4)
        }
        Stream.empty[Int]
      }

      "Then the generator should contains values in selected branches" in {
        generator should be(Seq(1, 2, 4))
      }

    }

    "Given a continuation that uses Yield" - {

      def yield4243: Continuation[Stream[Int], Unit] = _ {
        !Yield(42)
        !Yield(43)
      }

      "when create a generator that contains multiple Yield expression followed by a bang notation and a Stream.empty" - {

        def generator: Stream[Int] = {
          !Yield(0)
          !yield4243
          !Yield(1)
          Stream.empty[Int]
        }

        "Then the generator should contains yield values" in {
          generator should be(Seq(0, 42, 43, 1))
        }

      }

    }
  }
  "with exception handling support" - {
    "Given a continuation that throws an exception" - {
      object MyException extends Exception

      def continuation: Continuation[TryContinuation[Stream[Int]], Unit] = _ {
        !Yield(1)
        !Yield(2)
        throw MyException
      }

      def catching: Continuation[TryContinuation[Stream[Int]], Unit] = { onSucess =>
        TryContinuation { onFailure =>
          continuation(onSucess).underlying { e: Throwable =>
            onSucess(()).underlying(onFailure)
          }
        }
      }

    }

    "Given a generator" - {

      object MyException extends Exception
      def generator: TryContinuation[Stream[Int]] = {
        !Yield(1)
        !Yield(2)
        throw MyException
        !Yield(3)
        TryContinuation.success(Stream.empty)
      }
      "When catching exception thrown from the generator" - {
        val catching = generator.underlying { e: Throwable =>
          e should be(MyException)
          Stream(100)
        }
        "Then it should contain all elements before throwing exception and the element when catching the exception" in {
          catching should be(Seq(1, 2, 100))
        }
      }

    }

    "try/catch" in {

      def continuation: Continuation[TryContinuation[Stream[Int]], String] = _ {
        !Yield(0)
        val tryResult = try {
          !Yield(1)
          (0 / 0)
          !Yield(2)
          "try"
        } catch {
          case e: ArithmeticException =>
            !Yield(3)
            "catch"
        } finally {
          !Yield(4)
          "finally"
        }
        !Yield(5)
        "returns " + tryResult
      }

      continuation { result: String =>
        result should be("returns catch")
        TryContinuation.success(Stream.empty)
      }.underlying { e =>
        Stream.empty
      } should be(Seq(0, 1, 3, 4, 5))
    }
  }

}

object EachOpsSpec {
  type Continuation[R, +A] = (A => R) => R

  trait Lift[R0, R] {
    def lift[A](continuation: Continuation[R, A])(continue: A => R0): R0
  }

  private[EachOpsSpec] trait LowPriorityLift {
    implicit def liftContinuation[R0, R1, R](implicit liftRest: Lift[R0, R]): Lift[Continuation[R0, R1], R] =
      new Lift[Continuation[R0, R1], R] {
        def lift[A](continuation: Continuation[R, A])(continueA: (A) => Continuation[R0, R1]): Continuation[R0, R1] = {
          continue1: (R1 => R0) =>
            liftRest.lift(continuation) { a =>
              continueA(a)(continue1)
            }
        }
      }

    implicit def liftTry[R0, R](
        implicit liftContinuation: Lift[Continuation[R0, Throwable], R]): Lift[TryContinuation[R0], R] =
      new Lift[TryContinuation[R0], R] {
        def lift[A](continuation: Continuation[R, A])(continue: (A) => TryContinuation[R0]): TryContinuation[R0] = {
          TryContinuation(liftContinuation.lift[A](continuation) { a =>
            try {
              continue(a).underlying
            } catch {
              case NonFatal(e) =>
                _(e)
            }
          })
        }
      }

  }

  object Lift extends LowPriorityLift {

    implicit def liftIdentity[R]: Lift[R, R] = new Lift[R, R] {
      def lift[A](continuation: Continuation[R, A])(continue: (A) => R): R = continuation(continue)
    }

  }

  // TODO: Make it an opacity type
  final case class TryContinuation[R](underlying: Continuation[R, Throwable]) {

    def partialCatch(f: PartialFunction[Throwable, TryContinuation[R]]): TryContinuation[R] = {

      TryContinuation { failureHandler =>
        underlying { e =>
          object Extractor {
            def unapply(e: Throwable): Option[TryContinuation[R]] = f.lift(e)
          }

          e match {
            case Extractor(handled) => handled.underlying(failureHandler)
            case _                  => failureHandler(e)
          }
        }
      }
    }
  }

  object TryContinuation {
    def success[R](r: R): TryContinuation[R] = TryContinuation(Function.const(r))
    def failure[R](e: Throwable): TryContinuation[R] = TryContinuation(_(e))
  }

  implicit final class DependentThrowableContinuationOps[R, A](val underlying: Continuation[R, A])
      extends AnyVal
      with EachOps[A] {
//    def apply[R1, R0](continue: A => R1)(implicit lift: Lift[R0, R], constraint: R1 <:< R0): R0 = {
//      // FIXME: Use <:<.substitute instead of asInstanceOf for Scala 2.13
//      val substitution = continue.asInstanceOf[A => R0]
//      lift.lift(underlying)(substitution)
//    }
//
    def apply[R0](continue: A => R0)(implicit lift: Lift[R0, R]): R0 = {
      lift.lift(underlying)(continue)
    }
  }

  final case class Yield[Element](element: Element) extends Continuation[Stream[Element], Unit] {
    @inline override def apply(continue: Unit => Stream[Element]): Stream[Element] = {
      Stream.cons(element, continue(()))
    }
  }

}
