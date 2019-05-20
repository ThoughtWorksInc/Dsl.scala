package com.thoughtworks.dsl.keywords

import org.scalatest.{FreeSpec, Matchers}
import com.thoughtworks.dsl.Dsl.{!!, reset}

/**
  * @author 杨博 (Yang Bo)
  */
class EachSpec extends FreeSpec with Matchers {

  "reset helper" - {
    def resetReturnValue[A](a: A): A @reset = a
    def forceParameter[A](a: => A @reset): A = a

    "@reset parameter" ignore {
      val seq = 1 to 10
      def run(): Seq[Int] = Seq {
        val plus100 = forceParameter(Seq {
          !Each(seq) + 100
        })
        plus100.length should be(10)
        !Each(plus100)
      }

      val result = run()
      result.length should be(10)
      result.last should be(110)
    }

    "@reset block" in {
      val seq = 1 to 10
      def run(): Seq[Int] = Seq {
        val plus100 = resetReturnValue {
          Seq(!Each(seq) + 100)
        }
        plus100.length should be(10)
        !Each(plus100)
      }

      val result = run()
      result.length should be(10)
      result.last should be(110)
    }

    "@reset result value" ignore {
      val seq = 1 to 10
      def run(): Seq[Int] = Seq {
        val plus100 = {
          val element = !Each(seq)
          resetReturnValue {
            Seq(element + 100)
          }
        }
        plus100.length should be(1)
        !Each(plus100)
      }

      val result = run()
      result.length should be(10)
      result.last should be(110)
    }

  }

  "nested" - {

    "each" - {
      "explicit @reset" in {
        val seq = 1 to 10

        def run(): Seq[Int] = Seq {
          val plus100: Seq[Int] @reset = Seq {
            !Each(seq) + 100
          }
          plus100.length should be(1)
          !Each(plus100)
        }

        val result = run()
        result.length should be(10)
        result.last should be(110)
      }

      "val" in {
        val seq = 1 to 10

        def run(): Seq[Int] = Seq {
          val plus100 = Seq {
            !Each(seq) + 100
          }
          plus100.length should be(1)
          !Each(plus100)
        }

        val result = run()
        result.length should be(10)
        result.last should be(110)
      }

      "def" in {
        val seq = 1 to 10

        def run(): Seq[Int] = Seq {
          def plus100 = Seq {
            !Each(seq) + 100
          }
          plus100.length should be(10)
          !Each(plus100)
        }

        val result = run()
        result.length should be(10)
        result.last should be(110)
      }
    }
  }

  "default parameter" in {

    def foo(s: Seq[Int] = Seq {
      !Each(Seq(1, 2, 3)) + 100
    }) = s

    foo() should be(Seq(101, 102, 103))

  }

  "val in class" in {
    class C {
      val ascii: Set[Int] = Set(
        !Each(Seq(1, 2, 3, 2)) + 100
      )
    }

    (new C).ascii should be(Set(101, 102, 103))
  }

  "pattern matching" - {
    "val" in {
      def foo: Seq[String] =
        Seq {
          // OK
          val s0 = !Each(Seq("a"))

          // not OK
          val (s1, s2) = !Each(Seq(("b", "c")))
          s1
        }: @reset

      foo should be(Seq("b"))
    }
  }

  "Given a continuation that uses Yield and Each expressions" - {

    def asyncFunction: Stream[String] !! Unit = _ {
      !Yield("Entering asyncFunction")
      val subThreadId: Int = !Each(Seq(0, 1))
      !Yield(s"Fork sub-thread $subThreadId")
      !Yield("Leaving asyncFunction")
    }

    "When create a generator that contains Yield, Shift, and Each expressions" - {

      def generator: Stream[String] = {
        !Yield("Entering generator")
        val threadId = !Each(Seq(0, 1))
        !Yield(s"Fork thread $threadId")
        !Shift(asyncFunction)
        Stream("Leaving generator")
      }

      "Then the generator should contains yield values" in {
        generator should be(
          Seq(
            /**/ "Entering generator",
            /****/ "Fork thread 0",
            /******/ "Entering asyncFunction",
            /********/ "Fork sub-thread 0",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator",
            /********/ "Fork sub-thread 1",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator",
            /****/ "Fork thread 1",
            /******/ "Entering asyncFunction",
            /********/ "Fork sub-thread 0",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator",
            /********/ "Fork sub-thread 1",
            /**********/ "Leaving asyncFunction",
            /**********/ "Leaving generator"
          ))
      }

    }

  }

}
