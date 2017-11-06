package com.thoughtworks.each

import com.thoughtworks.each.Await.AsyncFunction
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
final class MayFailSpec extends FreeSpec with Matchers {
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

      def yield4243: AsyncFunction[Stream[Int], Unit] = _ {
        !Yield(42)
        !Yield(43)
      }

      "when create a generator that contains multiple Yield expression followed by a bang notation and a Stream.empty" - {

        def generator: Stream[Int] = {
          !Yield(0)
          !Await(yield4243)
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

      "Given a generator" - {

        object MyException extends Exception
        def generator: MayFail[Stream[Int]] = {
          !Yield(1)
          !Yield(2)
          throw MyException
          !Yield(3)
          MayFail.success(Stream.empty)
        }

        "When catching exception thrown from the generator" - {
          val catching = generator.onFailure { e: Throwable =>
            e should be(MyException)
            Stream(100)
          }
          "Then it should contain all elements before throwing exception and the element when catching the exception" in {
            catching should be(Seq(1, 2, 100))
          }
        }

      }
    }

    "try/catch" in {

      def continuation: AsyncFunction[MayFail[Stream[Int]], String] = _ {
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
        MayFail.success(Stream.empty)
      }.onFailure { e =>
        Stream.empty
      } should be(Seq(0, 1, 3, 4, 5))
    }
  }

}
