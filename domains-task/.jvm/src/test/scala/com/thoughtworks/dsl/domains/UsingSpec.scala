package com.thoughtworks.dsl
package domains

import com.thoughtworks.dsl.Dsl.{!!, Continuation, reset}
import com.thoughtworks.dsl.keywords.{Using, Yield}
import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** @author 杨博 (Yang Bo)
  */
class UsingSpec extends AnyFreeSpec with Matchers {

  def successContinuation[Domain](domain: Domain): (Domain !! Throwable) @reset = Continuation.empty(domain)

  "AutoCloseable" - {

    "scope" - {

      "arm" in {
        var isOpen = false

        def raii: Stream[Int] !! Throwable !! Assertion = Continuation.apply {
          !Yield(1)
          isOpen should be(false)
          val a = !Using {
            !Yield(2)
            new AutoCloseable {
              isOpen should be(false)
              isOpen = true

              def close(): Unit = {
                isOpen should be(true)
                isOpen = false
              }
            }
          }
          !Yield(3)
          isOpen should be(true)
        }

        isOpen should be(false)

        val myException = new Exception

        val stream = raii(_ => _ => Stream.empty)(throw _)

        stream should be(Stream(1, 2, 3))
        isOpen should be(false)
      }
    }

  }
}
