package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.instructions.Yield
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
final class ExceptionHandlingSpec extends FreeSpec with Matchers {
  type AsyncFunction[Domain, +A] = (A => Domain) => Domain

  "Given a continuation that throws an exception" - {
    object MyException extends Exception

    "Given a generator" - {

      object MyException extends Exception
      def generator: ExceptionHandling[Stream[Int]] = {
        !Yield(1)
        throw {
          !Yield(2)
          MyException
        }
        !Yield(3)
        ExceptionHandling.success(Stream.empty)
      }

      "When catching exception thrown from the generator" - {
        val catching = generator.apply { e: Throwable =>
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
      def continuation: AsyncFunction[ExceptionHandling[Stream[Int]], String] = _ {
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
        ExceptionHandling.success(Stream.empty)
      }.apply { e =>
        Stream.empty
      } should be(Seq(3))
    }

    "issue 2" in {
      def continuation: AsyncFunction[ExceptionHandling[Stream[Int]], String] = _ {
        !Yield(1)
        try {} catch {
          case e: ArithmeticException =>
            !Yield(2)
        }
        !Yield(3)
        0 / 0
        "OK"
      }

      continuation { result: String =>
        ExceptionHandling.failure(new AssertionError())
      }.apply { e =>
        e should be(a[ArithmeticException])
        Stream.empty
      } should be(Stream(1, 3))

    }

    "empty try" in {
      def continuation: AsyncFunction[ExceptionHandling[Stream[Int]], String] = _ {
        val tryResult = try {
          0 / 0
          !Yield(-1)
        } finally {}
        "returns " + tryResult
      }
      continuation { result: String =>
        ExceptionHandling.failure(new AssertionError())
      }.apply { e =>
        e should be(a[ArithmeticException])
        Stream.empty
      } should be(Seq())
    }
    "rethrow" in {
      def continuation: AsyncFunction[ExceptionHandling[Stream[Int]], String] = _ {
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
        ExceptionHandling.failure(new AssertionError())
      }.apply { e =>
        e should be(a[ArithmeticException])
        Stream.empty
      } should be(Seq(42))
    }

    "complex" in {
      def continuation: AsyncFunction[ExceptionHandling[Stream[Int]], String] = _ {
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
        ExceptionHandling.success(Stream.empty)
      }.apply { e =>
        Stream.empty
      } should be(Seq(0, 1, 3, 5, 6, 7))
    }
  }
}
