package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.Dsl.!!
import com.thoughtworks.dsl.domains.Raii
import com.thoughtworks.dsl.domains.Raii.{RaiiFailure, RaiiSuccess}
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class ArmSpec extends FreeSpec with Matchers {
  private def successContinuation[Domain](domain: Domain): Domain !! Raii = _ {
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

    type Reset[A] = A !! A
    def noop[A](a: A): Reset[A] = _(a)

    "scope" - {

      "arm" in {
        var isOpen = false

        def raii: Reset[Stream[Int] !! Raii] = noop {
          successContinuation {

            !Yield(1)
            isOpen should be(false)
            val a = !Arm {
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
            Stream.empty
          }
        }

        isOpen should be(false)

        val myException = new Exception
        val stream = raii(identity).onFailure { e =>
          throw e
        }

        stream should be(Stream(1, 2, 3))
        isOpen should be(false)
      }
    }

  }
}
