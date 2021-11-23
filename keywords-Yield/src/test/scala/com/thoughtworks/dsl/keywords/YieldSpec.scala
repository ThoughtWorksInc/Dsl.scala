package com.thoughtworks.dsl
package keywords
import bangnotation._
import com.thoughtworks.dsl.Dsl.!!
import scala.annotation.tailrec
import scala.collection.{LinearSeq, SeqView}
import scala.runtime.NonLocalReturnControl
import scala.language.implicitConversions
import utest.{* => _, _}

/**
  * @author 杨博 (Yang Bo)
  */
@deprecated("This test suite contains test cases for Stream, which is deprecated")
object YieldSpec extends TestSuite {

  val tests = Tests {

    reset {
      !Yield(1)

      def nested(): Stream[Int] = reset {
        !Yield(2)
        Stream.empty[Int]
      }

      nested()
    } // Should compile

    // "Given a continuation that uses Yield and Each expressions" - {

    //   def asyncFunction: Stream[String] !! Unit = _ {
    //     !Yield("Entering asyncFunction")
    //     val subThreadId: Int = !Each(Seq(0, 1))
    //     !Yield(s"Fork sub-thread $subThreadId")
    //     !Yield("Leaving asyncFunction")
    //   }

    //   "When create a generator that contains Yield, Shift, and Each expressions" - {

    //     def generator: Stream[String] = {
    //       !Yield("Entering generator")
    //       val threadId = !Each(Seq(0, 1))
    //       !Yield(s"Fork thread $threadId")
    //       !Shift(asyncFunction)
    //       Stream("Leaving generator")
    //     }

    //     "Then the generator should contains yield values" - {
    //       assert(generator ==
    //         Seq(
    //           /**/ "Entering generator",
    //           /****/ "Fork thread 0",
    //           /******/ "Entering asyncFunction",
    //           /********/ "Fork sub-thread 0",
    //           /**********/ "Leaving asyncFunction",
    //           /**********/ "Leaving generator",
    //           /********/ "Fork sub-thread 1",
    //           /**********/ "Leaving asyncFunction",
    //           /**********/ "Leaving generator",
    //           /****/ "Fork thread 1",
    //           /******/ "Entering asyncFunction",
    //           /********/ "Fork sub-thread 0",
    //           /**********/ "Leaving asyncFunction",
    //           /**********/ "Leaving generator",
    //           /********/ "Fork sub-thread 1",
    //           /**********/ "Leaving asyncFunction",
    //           /**********/ "Leaving generator"
    //         ))
    //     }

    //   }

    // }

    "stream" - {

      def shouldCompile = reset {
        !Yield("naked")
        Stream.empty[String]
      }

      "local method" - {
        def generator: Stream[Int] = reset {
          def id[A](a: A) = a
          id(!Yield(100))
          Stream.empty[Int]
        }
        assert(generator == Stream(100))
      }

      "yield from" - {
        def generator = reset[Stream[Int]] {
          def id[A](a: A) = a
          id(!Yield(100, 200))
          Stream.empty
        }
        assert(generator == Stream(100, 200))
      }

      "local function" - {
        def generator: Stream[Int] = reset {
          def id[A](a: A) = a
          (id[Unit] _)(!Yield(100))
          Stream.empty[Int]
        }
        assert(generator ==Stream(100))
      }

      "do/while" - {
        "empty body" - {
          def generator: Stream[Int] = reset {
            while {
              !Yield(100)
              false
            } do ()
            Stream.empty[Int]
          }
          assert(generator ==Stream(100))
        }

        "false" - {
          def generator: Stream[Int] = reset {
            while {
              !Yield(100)
              false
            } do ()
            Stream.empty[Int]
          }
          assert(generator ==Stream(100))
        }

        "with var" - {
          def generator: Stream[Int] = reset {
            var i = 5
            while {
              i -= {
                !Yield(i)
                1
              }
              !Yield(-i)
              i > 0
            } do()
            Stream.empty[Int]
          }
          assert(generator ==Stream(5, -4, 4, -3, 3, -2, 2, -1, 1, 0))
        }
      }

      "while" - {
        "false" - {
          def whileFalse: Stream[Int] = reset {
            while (false) {
              !Yield(100)
            }
            Stream.empty[Int]
          }

          assert(whileFalse == Stream.empty)
        }
      }

      "match/case" - {

        def loop(i: Int): Stream[Int] = reset {
          i match {
            case 100 =>
              Stream.empty
            case _ =>
              !Yield(i)
              loop(i + 1)
          }
        }

        assert(loop(90) == Stream(90, 91, 92, 93, 94, 95, 96, 97, 98, 99))
      }

      "recursive" - {
        def loop(i: Int): Stream[Int] = reset {
          if (i < 100) {
            !Yield(i)
            loop(i + 1)
          } else {
            Stream.empty
          }
        }

        assert(loop(90) == Stream(90, 91, 92, 93, 94, 95, 96, 97, 98, 99))

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
          Stream.empty[Int]
        }

        "Then the generator should contains values in selected branches" - {
          assert(generator == Seq(1, 2, 4))
        }

      }

    //   "Given a continuation that uses Yield" - {

    //     def yield4243: Stream[Int] !! Unit = _ {
    //       !Yield(42)
    //       !Yield(43)
    //     }

    //     "when create a generator that contains multiple Yield expression followed by a bang notation and a Stream.empty" - {

    //       def generator: Stream[Int] = {
    //         !Yield(0)
    //         !Shift(yield4243)
    //         !Yield(1)
    //         Stream.empty[Int]
    //       }

    //       "Then the generator should contains yield values" - {
    //         assert(generator ==Seq(0, 42, 43, 1))
    //       }

    //     }

    //   }

      "apply" - {
        def generator = reset[Stream[Int]] {
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
        assert(generator == Stream(1, 2, -42))
      }

      "return" - {
        def returnGenerator: Stream[Int] = reset {
          if (true) {
            return {
              !Yield(100)
              Stream(42)
            }
          }
          Stream.empty[Int]
        }
        assert(returnGenerator == List(100, 42))
      }
      "partial function" - {
        "empty" - {
          assert(Seq.empty[Any].flatMap {
            case i: Int =>
              reset {
                !Yield(100)
                Stream(42)
              }
          } == Seq.empty)
        }

        "flatMap" - {
          def flatMapReset = Seq(100, 200).flatMap {
            case i: Int =>
              reset {
                !Yield(100)
                Stream(42 + i)
              }
          }
          assert(flatMapReset == Seq(100, 142, 100, 242))
        }
      }

      "nested function call" - {
        "call by value" - {
          def nested() = reset {
            "foo" +: !Yield("bar") +: Stream.empty[Any]
          }
          assert(nested() == Stream("bar", "foo", ()))
        }
        "call by name" - {
          def nested() = reset {
            "foo" #:: !Yield("bar") #:: Stream.empty[Any]
          }
          assert(nested() == Stream("bar", "foo", ()))
        }
      }

    }

    "view" - {

      "indexed seq view" - {
        def generator = reset {
          !Yield("naked")
          Vector.empty[String].view
        }
        assert(generator.toList == List("naked"))
      }
  
      "yield from indexed seq view" - {
        def generator = reset {
          !Yield(100, 101)
          Vector.empty[Int].view
        }
        assert(generator.toList == List(100, 101))
      }
  
      "yield from seq view" - {
        def generator = reset {
          !Yield(100, 101)
          Seq.empty[Int].view
        }
        assert(generator.toList == List(100, 101))
      }

      "local method" - {
        def generator = reset {
          def id[A](a: A) = a
          id(!Yield(100))
          Seq.empty[Int].view
        }
        assert(generator.toList == List(100))
      }

      test("yield from") {
        def generator = reset {
          def id[A](a: A) = a
          id(!Yield(100, 200))
          Seq.empty[Int].view ++ Nil
        }
        assert(generator.toList == List(100, 200))
      }

    }

    // "iterator" - {

    //   def shouldCompile: Iterator[String] = {
    //     !Yield("naked")
    //     Iterator.empty
    //   }

    //   "local method" - {
    //     def generator: Iterator[Int] = {
    //       def id[A](a: A) = a
    //       id(!Yield(100))
    //       Iterator.empty
    //     }
    //      assert(generator.toList == List(100))
    //   }

    //   "yield from" - {
    //     def generator: Iterator[Int] = {
    //       def id[A](a: A) = a
    //       id(!Yield(100, 200))
    //       Iterator.empty
    //     }
    //      assert(generator.toList == List(100, 200))
    //   }
    // }

    // "seq" - {

    //   def shouldCompile: LinearSeq[String] = {
    //     !Yield("naked")
    //     LinearSeq.empty[String]
    //   }

    //   "local method" - {
    //     def generator: LinearSeq[Int] = {
    //       def id[A](a: A) = a
    //       id(!Yield(100))
    //       LinearSeq.empty
    //     }
    //     assert(generator ==LinearSeq(100))
    //   }

    //   "yield from" - {
    //     def generator: LinearSeq[Int] = {
    //       def id[A](a: A) = a
    //       id(!Yield(100, 200))
    //       LinearSeq.empty
    //     }
    //     assert(generator ==LinearSeq(100, 200))
    //   }
    // }
  }
}