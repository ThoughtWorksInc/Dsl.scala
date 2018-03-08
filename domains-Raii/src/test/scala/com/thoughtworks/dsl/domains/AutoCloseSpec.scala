package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.domains.Raii.{RaiiFailure, RaiiSuccess}
import com.thoughtworks.dsl.keywords.{AutoClose, Yield}
import org.scalatest.{Assertion, FreeSpec, Matchers}

import scala.util.{Failure, Success}

/**
  * @author 杨博 (Yang Bo)
  */
class AutoCloseSpec extends FreeSpec with Matchers {
  private def successContinuation[Domain](domain: Domain): (Domain !! Raii) = _ {
    new RaiiSuccess[Domain] {
      def continue(): Domain = domain
    }
  }

  private implicit final class OnFailureOps[Domain](raiiContinuation: Domain !! Raii) {
    def onFailure(failureHandler: Throwable => Domain): Domain = {
      raiiContinuation {
        case RaiiFailure(e) =>
          failureHandler(e)
        case returning: RaiiSuccess[Domain] @unchecked =>
          returning.continue()
      }
    }
  }

  "AutoCloseable" - {

    "scope" - {

      "arm" in {
        var isOpen = false

        def raii: Stream[Int] !! Raii !! Assertion = _ {
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

        val stream = raii.run {
          case Failure(e) =>
            throw e
          case Success(s) =>
            Stream.empty
        }

        stream should be(Stream(1, 2, 3))
        isOpen should be(false)
      }
    }

  }
}
