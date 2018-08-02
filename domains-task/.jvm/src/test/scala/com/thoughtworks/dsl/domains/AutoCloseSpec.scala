package com.thoughtworks.dsl
package domains

import com.thoughtworks.dsl.Dsl.{!!, Continuation, reset}
import com.thoughtworks.dsl.keywords.{AutoClose, Yield}
import org.scalatest.{Assertion, FreeSpec, Matchers}
import com.thoughtworks.dsl.domains.task._

import scala.util.{Failure, Success}

/**
  * @author 杨博 (Yang Bo)
  */
class AutoCloseSpec extends FreeSpec with Matchers {

  def successContinuation[Domain](domain: Domain): (Domain !! Throwable) @reset = Continuation.empty(domain)

  "AutoCloseable" - {

    "scope" - {

      "arm" in {
        var isOpen = false

        def raii: Stream[Int] !! Throwable !! Assertion = Continuation.apply {
          !Yield(1)
          isOpen should be(false)
          val a = !AutoClose {
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
