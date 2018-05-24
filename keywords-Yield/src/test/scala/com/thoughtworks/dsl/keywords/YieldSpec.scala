package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl.!!
import org.scalatest.{FreeSpec, Matchers}

import scala.annotation.tailrec
import scala.runtime.NonLocalReturnControl

/**
  * @author 杨博 (Yang Bo)
  */
class YieldSpec extends FreeSpec with Matchers {

  {
    !Yield("naked")
    Stream.empty[String]
  } // should compile

  "local method" in {
    def generator: Stream[Int] = {
      def id[A](a: A) = a
      id(!Yield(100))
      Stream.empty[Int]
    }
    generator should be(Stream(100))
  }

  "local function" in {
    def generator: Stream[Int] = {
      def id[A](a: A) = a
      (id[Unit] _)(!Yield(100))
      Stream.empty[Int]
    }
    generator should be(Stream(100))
  }

  "do/while" - {
    "empty body" in {
      def generator: Stream[Int] = {
        do {} while ({
          !Yield(100)
          false
        })
        Stream.empty[Int]
      }
      generator should be(Stream(100))
    }

    "false" in {
      def generator: Stream[Int] = {
        do {
          !Yield(100)
        } while (false)
        Stream.empty[Int]
      }
      generator should be(Stream(100))
    }

    "with var" in {
      def generator: Stream[Int] = {
        var i = 5
        do {
          i -= {
            !Yield(i)
            1
          }
        } while ({
          !Yield(-i)
          i > 0
        })
        Stream.empty[Int]
      }
      generator should be(Stream(5, -4, 4, -3, 3, -2, 2, -1, 1, 0))
    }
  }

  "while" - {
    "false" in {
      def whileFalse: Stream[Int] = {
        while (false) {
          !Yield(100)
        }
        Stream.empty[Int]
      }

      whileFalse should be(Stream.empty)
    }
  }

  "match/case" in {

    def loop(i: Int): Stream[Int] = {
      i match {
        case 100 =>
          Stream.empty
        case _ =>
          !Yield(i)
          loop(i + 1)
      }
    }

    loop(90) should be(Stream(90, 91, 92, 93, 94, 95, 96, 97, 98, 99))

  }

  "recursive" in {
    def loop(i: Int): Stream[Int] = {
      if (i < 100) {
        !Yield(i)
        loop(i + 1)
      } else {
        Stream.empty
      }
    }

    loop(90) should be(Stream(90, 91, 92, 93, 94, 95, 96, 97, 98, 99))

  }

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

    def yield4243: Stream[Int] !! Unit = _ {
      !Yield(42)
      !Yield(43)
    }

    "when create a generator that contains multiple Yield expression followed by a bang notation and a Stream.empty" - {

      def generator: Stream[Int] = {
        !Yield(0)
        !Shift(yield4243)
        !Yield(1)
        Stream.empty[Int]
      }

      "Then the generator should contains yield values" in {
        generator should be(Seq(0, 42, 43, 1))
      }

    }

  }

  "apply" in {
    def generator: Stream[Int] = {
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
      Stream(result)
    }
    generator should be(Stream(1, 2, -42))
  }

  "return" in {
    def generator: Stream[Int] = {
      if (true) {
        return {
          !Yield(100)
          Stream(42)
        }
      }
      Stream.empty[Int]
    }

    a[NonLocalReturnControl[Stream[Int]]] should be thrownBy generator.last
  }
  "partial function" - {
    "empty" in {
      Seq.empty[Any].flatMap {
        case i: Int =>
          !Yield(100)
          Stream(42)
      } should be(empty)
    }

    "flatMap" in {
      Seq(100, 200).flatMap {
        case i: Int =>
          !Yield(100)
          Stream(42 + i)
      } should be(Seq(100, 142, 100, 242))
    }
  }

  "nested function call" - {
    "call by value" in {
      def nested() = {
        "foo" +: !Yield("bar") +: Stream.empty[Any]
      }
      nested() should be(Stream("bar", "foo", ()))
    }
    "call by name" in {
      def nested() = {
        "foo" #:: !Yield("bar") #:: Stream.empty[Any]
      }
      nested() should be(Stream("bar", "foo", ()))
    }
  }

}
