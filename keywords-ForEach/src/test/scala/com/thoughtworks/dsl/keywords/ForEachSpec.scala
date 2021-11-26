package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl.{!!, reset}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** @author 杨博 (Yang Bo)
  */
class ForEachSpec extends AnyFreeSpec with Matchers {

  "foreach" - {

    "val" in {
      val seq = 1 to 10

      def run(): Unit = {
        val plus100 = Seq {
          !ForEach(seq) + 100
        }
        plus100.length should be(1)
        !ForEach(plus100)
      }

      run()
    }
    "def" in {
      val seq = 1 to 10

      def run(): Unit = {
        def plus100 = Seq {
          !Each(seq) + 100
        }
        plus100.length should be(10)
        !ForEach(plus100)
      }

      run()
    }
  }
}
