package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.{!!, Continuation, reset}
import com.thoughtworks.dsl.keywords.{Catch, Scope, Yield}
import org.scalatest.{FreeSpec, Matchers}
import com.thoughtworks.dsl.task._

/**
  * @author 杨博 (Yang Bo)
  */
final class RaiiSpec extends FreeSpec with Matchers {

  /**
    * Exit the current scope then hang up
    */
  def successContinuation[Domain](domain: Domain): (Domain !! Throwable) @reset = Continuation.empty(domain)

  def failureContinuation[Domain](throwable: Throwable): (Domain !! Throwable) @reset = Continuation.now(throwable)

  "Given a continuation that throws an exception" - {
    object MyException extends Exception

    "Given a generator" - {

      object MyException extends Exception
      def generator: Stream[Int] !! Throwable = {
        !Yield(1)
        throw {
          !Yield(2)
          MyException
        }
        !Yield(3)
        successContinuation(Stream.empty)
      }

      "When catching exception thrown from the generator" - {
        val catching = generator { e: Throwable =>
          e should be(MyException)
          Stream(100)
        }
        "Then it should contain all elements before throwing exception and the element when catching the exception" in {
          catching should be(Seq(1, 2, 100))
        }
      }

    }
  }
  "try/catch" - {
    "yield and catch" in {
      def continuation: Stream[Int] !! Throwable !! String = _ {
        val tryResult = try {
          0 / 0
        } catch {
          case e: ArithmeticException =>
            !Yield(3)
            "catch"
        }
        "returns " + tryResult
      }
      continuation { result: String =>
        result should be("returns catch")
        successContinuation(Stream.empty)
      } { e =>
        Stream.empty
      } should be(Seq(3))
    }

    "simple catch" in {

      object MyException extends Exception
      def generator: Stream[String] !! Throwable !! Int = { continue =>
        !Yield("before catch")

        !Catch { e: Throwable =>
          e should be(MyException)
          !Yield("catch")
          continue(42)
        }
        !Yield("after catch")
        continue(43)
      }

      object MyException2 extends Exception

      def generator2: Stream[String] !! Throwable = {
        import Throwable._
        val i = !Scope(generator)
        !Yield(i.toString)
        i should be(43)
        failureContinuation(MyException)
      }

      generator2 { e =>
        e should be(MyException)

        Stream("end")
      } should be(Stream("before catch", "after catch", "43", "end"))
    }

    "issue 2" in {
      def continuation: Stream[Int] !! Throwable !! String = { continue =>
        !Yield(1)
        try {} catch {
          case e: ArithmeticException =>
            !Yield(2)
        }
        !Yield(3)
        failureContinuation(new ArithmeticException)
      }

      continuation { result: String =>
        failureContinuation(new AssertionError())
      } { e =>
        e should be(a[ArithmeticException])
        Stream.empty
      } should be(Stream(1, 3))

    }

    // FIXME:
    "empty try" ignore {
      def continuation: Stream[Int] !! Throwable !! String = _ {
        val tryResult = try {
          0 / 0
          !Yield(-1)
        } finally {}
        "returns " + tryResult
      }

      jvmCatch(continuation { result: String =>
        failureContinuation(new AssertionError())
      }) { e =>
        e should be(a[ArithmeticException])
        Stream.empty
      } should be(Seq())
    }
    "rethrow" in {
      def continuation: Stream[Int] !! Throwable !! String = _ {
        val tryResult = try {
          0 / 0
        } catch {
          case e: ArithmeticException =>
            !Yield(42)
            throw e
        }
        "returns " + tryResult
      }
      continuation { result: String =>
        failureContinuation(new AssertionError())
      } { e =>
        e should be(a[ArithmeticException])
        Stream.empty
      } should be(Seq(42))
    }

    "complex" in {
      def continuation: Stream[Int] !! Throwable !! String = _ {
        !Yield(0)
        val tryResult = try {
          !Yield(1)
          try {} catch {
            case e: ArithmeticException =>
              !Yield(2)
          }
          !Yield(3)

          0 / 0
          !Yield(4)
          "try"
        } catch {
          case e: ArithmeticException =>
            !Yield(5)
            "catch"
        } finally {
          !Yield(6)

          def ignoredFinalResult = "finally"
          ignoredFinalResult
        }
        !Yield(7)
        "returns " + tryResult
      }

      continuation { result: String =>
        result should be("returns catch")
        successContinuation(Stream.empty)
      } { e =>
        Stream.empty
      } should be(Seq(0, 1, 3, 5, 6, 7))
    }
  }
  //
//  "Given a continuation that throws an exception" - {
//    object MyException extends Exception
//
//    "Given a generator" - {
//
//      object MyException extends Exception
//      def generator: Throwable[Stream[Int]] = {
//        !Yield(1)
//        throw {
//          !Yield(2)
//          MyException
//        }
//        !Yield(3)
//        Throwable.success(Stream.empty)
//      }
//
//      "When catching exception thrown from the generator" - {
//        val catching = generator.apply { e: Throwable =>
//          e should be(MyException)
//          Stream(100)
//        }
//        "Then it should contain all elements before throwing exception and the element when catching the exception" in {
//          catching should be(Seq(1, 2, 100))
//        }
//      }
//
//    }
//  }
//
//  "try/catch" - {
//    "yield and catch" in {
//      def continuation: Continuation[Throwable[Stream[Int]], String] = _ {
//        val tryResult = try {
//          0 / 0
//        } catch {
//          case e: ArithmeticException =>
//            !Yield(3)
//            "catch"
//        }
//        "returns " + tryResult
//      }
//
//      continuation.onComplete { r =>
//        r.get should be("returns catch")
//
//        Stream(5)
//
//      } should be(Stream(3, 5))
//    }
//  }

}
