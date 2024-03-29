package com.thoughtworks.dsl
package keywords

import com.thoughtworks.dsl.macros.Reset.Default.*

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.collection.{LinearSeq, SeqView}
import scala.runtime.NonLocalReturnControl

/** @author
  *   杨博 (Yang Bo)
  */
class YieldSpec213 extends AnyFreeSpec with Matchers {

  "lazylist" - {

    def shouldCompile = reset {
      !Yield("naked")
      LazyList.empty[String]
    }

    "local method" in {
      def generator: LazyList[Int] = reset {
        def id[A](a: A) = a
        id(!Yield(100))
        LazyList.empty[Int]
      }
      generator should be(LazyList(100))
    }

    "yield from" in {
      def generator = reset[LazyList[Int]] {
        def id[A](a: A) = a
        id(!Yield(100, 200))
        LazyList.empty
      }
      generator should be(LazyList(100, 200))
    }

    "local function" in {
      def generator: LazyList[Int] = reset {
        def id[A](a: A) = a
        (id[Unit] _)(!Yield(100))
        LazyList.empty[Int]
      }
      generator should be(LazyList(100))
    }

    "do/while" - {
      "empty body" in {
        def generator: LazyList[Int] = reset {
          while {
            !Yield(100)
            false
          } do ()
          LazyList.empty[Int]
        }
        generator should be(LazyList(100))
      }

      "false" in {
        def generator: LazyList[Int] = reset {
          while {
            !Yield(100)
            false
          } do ()
          LazyList.empty[Int]
        }
        generator should be(LazyList(100))
      }

      "with var" in {
        def generator: LazyList[Int] = reset {
          var i = 5
          while {
            i -= {
              !Yield(i)
              1
            }
            !Yield(-i)
            i > 0
          } do ()
          LazyList.empty[Int]
        }
        generator should be(LazyList(5, -4, 4, -3, 3, -2, 2, -1, 1, 0))
      }
    }

    "while" - {
      "false" in {
        def whileFalse: LazyList[Int] = reset {
          while (false) {
            !Yield(100)
          }
          LazyList.empty[Int]
        }

        whileFalse should be(LazyList.empty)
      }
    }

    "match/case" in {

      def loop(i: Int): LazyList[Int] = reset {
        i match {
          case 100 =>
            LazyList.empty
          case _ =>
            !Yield(i)
            loop(i + 1)
        }
      }

      loop(90) should be(LazyList(90, 91, 92, 93, 94, 95, 96, 97, 98, 99))

    }

    "recursive" in {
      def loop(i: Int): LazyList[Int] = reset {
        if (i < 100) {
          !Yield(i)
          loop(i + 1)
        } else {
          LazyList.empty
        }
      }

      loop(90) should be(LazyList(90, 91, 92, 93, 94, 95, 96, 97, 98, 99))

    }

    "Given a generator that contains conditional Yield" - {
      def generator = reset {
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
        LazyList.empty[Int]
      }

      "Then the generator should contains values in selected branches" in {
        generator should be(Seq(1, 2, 4))
      }

    }

    "Given a continuation that uses Yield" - {

      def yield4243: LazyList[Int] !! Unit = *[[X] =>> LazyList[Int] !! X] {
        !Yield(42)
        !Yield(43)
      }

      "when create a generator that contains multiple Yield expression followed by a bang notation and a LazyList.empty" - {

        def generator: LazyList[Int] = reset {
          !Yield(0)
          !Shift(yield4243)
          !Yield(1)
          LazyList.empty[Int]
        }

        "Then the generator should contains yield values" in {
          generator should be(Seq(0, 42, 43, 1))
        }

      }

    }

    "apply" in {
      def generator: LazyList[Int] = reset {
        val f = {
          !Yield(1)

          { (x: Int) =>
            -x
          }
        }

        val result = f({
          !Yield(2)
          42
        })
        LazyList(result)
      }
      generator should be(LazyList(1, 2, -42))
    }

    "return" in {
      def generator: LazyList[Int] = reset {
        if (true) {
          return {
            !Yield(100)
            LazyList(42)
          }
        }
        LazyList.empty[Int]
      }

      a[NonLocalReturnControl[LazyList[Int]]] should be thrownBy generator.last
    }
    "partial function" - {
      "empty" in {
        Seq.empty[Any].flatMap { case i: Int =>
          reset {
            !Yield(100)
            LazyList(42)
          }
        } should be(empty)
      }

      "flatMap" in {
        Seq(100, 200).flatMap { case i: Int =>
          reset {
            !Yield(100)
            LazyList(42 + i)
          }
        } should be(Seq(100, 142, 100, 242))
      }
    }

    "nested function call" - {
      "call by value" in {
        def nested() = reset {
          "foo" +: !Yield("bar") +: LazyList.empty[Any]
        }
        nested() should be(LazyList("bar", "foo", ()))
      }
      "call by name" in {
        def nested() = reset {
          "foo" #:: !Yield("bar") #:: LazyList.empty[Any]
        }
        nested() should be(LazyList("bar", "foo", ()))
      }
    }

  }

}
