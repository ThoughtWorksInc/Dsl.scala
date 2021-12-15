package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl.!!
import org.scalatest.freespec.AnyFreeSpec
import com.thoughtworks.dsl.reset, reset._
import org.scalatest.matchers.should.Matchers

/** @author 杨博 (Yang Bo)
  */
class ForEachSpec extends AnyFreeSpec with Matchers {

  "foreach" - {

    "val" in {
      val seq = 1 to 10

      def run(): Unit = reset[Unit] {
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

      def run(): Unit = reset[Unit] {
        def plus100 = reset apply Seq(
          !Each(seq) + 100
        )
        plus100.length should be(10)
        !ForEach(plus100)
      }

      run()
    }
  }
}
