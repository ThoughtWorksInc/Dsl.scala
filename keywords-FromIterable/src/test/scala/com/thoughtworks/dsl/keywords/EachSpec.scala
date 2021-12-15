package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.reset, reset._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** @author
  *   杨博 (Yang Bo)
  */
class EachSpec extends AnyFreeSpec with Matchers {

  "reset helper" - {

    "@reset parameter" in {
      val seq = 1 to 10
      def run(): Seq[Int] = reset(Seq {
        val plus100 = reset(Seq {
          !FromIterable(seq) + 100
        })
        plus100.length should be(10)
        !FromIterable(plus100)
      })

      val result = run()
      result.length should be(10)
      result.last should be(110)
    }

    "reset block" in {
      val seq = 1 to 10
      def run(): Seq[Int] = reset(Seq {
        val plus100 = reset {
          Seq(!FromIterable(seq) + 100)
        }
        plus100.length should be(10)
        !FromIterable(plus100)
      })

      val result = run()
      result.length should be(10)
      result.last should be(110)
    }

    "block without reset should behave the same as a block with a reset" in {
      val seq = 1 to 10
      def run(): Seq[Int] = reset {
        val plus100 = {
          val element = !FromIterable(seq)
          Seq(element + 100)
        }
        plus100.length should be(10)
        Seq(!FromIterable(plus100))
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

        def run(): Seq[Int] = reset {
          val plus100: Seq[Int] = Seq {
            !FromIterable(seq) + 100
          }
          plus100.length should be(1)
          Seq(!FromIterable(plus100))
        }

        val result = run()
        result.length should be(10)
        result.last should be(110)
      }

      "val" in {
        val seq = 1 to 10

        def run(): Seq[Int] = reset {
          val plus100 = Seq {
            !FromIterable(seq) + 100
          }
          plus100.length should be(1)
          Seq(!FromIterable(plus100))
        }

        val result = run()
        result.length should be(10)
        result.last should be(110)
      }

      "def" in {
        val seq = 1 to 10

        def run(): Seq[Int] = reset {
          def plus100 = reset apply Seq {
            !FromIterable(seq) + 100
          }
          plus100.length should be(10)
          Seq(!FromIterable(plus100))
        }

        val result = run()
        result.length should be(10)
        result.last should be(110)
      }
    }
  }

  "default parameter" in {

    def foo(s: Seq[Int] = reset {
      Seq(!FromIterable(Seq(1, 2, 3)) + 100)
    }) = s

    foo() should be(Seq(101, 102, 103))

  }

  "val in class" in {
    class C {
      val ascii: Set[Int] = reset(
        Set(!FromIterable(Seq(1, 2, 3, 2)) + 100)
      )
    }

    (new C).ascii should be(Set(101, 102, 103))
  }

  "pattern matching" - {
    "val" in {
      def foo: Seq[String] =
        reset {
          // OK
          val s0 = !FromIterable(Seq("a"))

          // How to support the following use case?
          // val (s1, s2) = !FromIterable(Seq(("b", "c"))) 

          val Seq(s1, s2) = !FromIterable(Seq(Seq("b", "c")))
          Seq(s1)
        }

      foo should be(Seq("b"))
    }
  }

}
