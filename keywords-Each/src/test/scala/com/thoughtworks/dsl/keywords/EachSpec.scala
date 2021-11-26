package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl.{!!, reset}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** @author 杨博 (Yang Bo)
  */
class EachSpec extends AnyFreeSpec with Matchers {

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

}
