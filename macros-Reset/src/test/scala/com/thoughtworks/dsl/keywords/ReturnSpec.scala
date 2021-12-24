package com.thoughtworks.dsl
package keywords
import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.macros.Reset
import com.thoughtworks.dsl.macros.Reset.Default.*
import utest.{TestSuite, Tests, given}
import Dsl.Run
import scala.language.implicitConversions
import Dsl.IsKeyword
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
object ReturnSpec extends TestSuite {

  val tests = Tests {
    summon[Run[Pure[Int], LazyList[Int], Int]]
    summon[Run[Return[Int], LazyList[Int], Nothing]]

    "return a LazyList" - {
      val stream1 = reset { !Return[Int](1) : LazyList[Int] }
      val stream2 = reset[LazyList[Int]] { !Return[Int](1) }
      val stream3 = *[LazyList].apply { 1 }
      val stream4: LazyList[Int] = *[LazyList].apply { !Return[Int](1) : Int }
      summon[stream1.type <:< LazyList[Int]]
      summon[util.NotGiven[stream1.type <:< Int]]
      summon[stream2.type <:< LazyList[Int]]
      summon[util.NotGiven[stream2.type <:< Nothing]]
      summon[stream3.type  <:< LazyList[Int]]
      summon[util.NotGiven[stream3.type <:< LazyList[Nothing]]]
      assert(stream1 == LazyList(1))
      assert(stream2 == LazyList(1))
      assert(stream3 == LazyList(1))
      assert(stream4 == LazyList(1))
    }
    "return a Iterable" - {
      def iterable: Iterable[Int] = *[Iterable] { !Pure(1)  }
      assert(iterable == Iterable(1))
    }

    "return the left domain" - {
      def continuation = reset[Int !! String] { !Return(42) }
      val result = continuation { s =>
        throw new java.lang.AssertionError(s)
      }
      assert(result == 42)
    }

    "reset nested function" - {
      new Reset {
        type ShouldResetNestedFunctions = true
      }.reset {
        def continuation: Int !! String = { !Return(42) }
        val result = continuation { s =>
          throw new java.lang.AssertionError(s)
        }
        assert(result == 42)
      }
    }

    "return the right domain" - {
      def continuation: Int !! String = reset[Int !! String]{!Return("right value") }

      assert(continuation { s =>
        assert(s == "right value")
        43
      } == 43)
    }

    "return the middle domain" - {

      "as the return value" - {
        def continuation: Int !! Double !! String = reset[Int !! Double !! String] { !Return(1.23) }
        assert(continuation { s =>
          ???
        } { d =>
          assert(d == 1.23)
          43
        } == 43)
      }

      "then the throw expression will not be executed" - {
        def continuation: Int !! Double !! String = reset[Int !! Double !! String] {
          throw !Return(1.23)
        }

        assert(continuation { s =>
          ???
        } { d =>
          assert(d == 1.23)
          43
        } == 43)
      }

      "summon Dsl.Run" - {
        summon[ Dsl.Run[com.thoughtworks.dsl.keywords.FlatMap[
          com.thoughtworks.dsl.keywords.If[
            com.thoughtworks.dsl.keywords.Pure$package.Pure[scala.Boolean],
            com.thoughtworks.dsl.keywords.Suspend$package.Suspend[com.thoughtworks.dsl.keywords.Pure$package.Pure[scala.Double]],
            com.thoughtworks.dsl.keywords.Suspend$package.Suspend[com.thoughtworks.dsl.keywords.Pure$package.Pure[scala.Double]]
          ], com.thoughtworks.dsl.keywords.Pure$package.Pure[scala.Double]], Double !! Double, Double ]]
      }
      "condition" - {
        val continuation = *[[X] =>> AnyRef !! X] {
          val b = true
          val c = "my string"
          val d = new StringBuilder
          if (b) {
            c
          } else {
            d
          }
        }

        assert(continuation(identity) == "my string")
      }
      "match / case " - {
        val continuation = *[[X] =>> AnyRef !! X] {
          val b = true
          val c = "my string"
          val d = new StringBuilder
          b match {
            case true =>
              "my string"
            case false =>
              d
          }
        }

        assert(continuation(identity) == "my string")
      }

      "try / catch" - {
        val continuation = *[[X] =>> Unit !! Throwable !! X] {
          val c = "my string"
          val d = new StringBuilder
          try {
            c
          } catch {
            case NonFatal(e) =>
              d
          }
        }

      }
    }

  }
}
