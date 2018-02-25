package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.domains.{ExceptionHandling, Scope}
import com.thoughtworks.dsl.instructions.Shift.Continuation
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class ArmSpec extends FreeSpec with Matchers {

  "AutoCloseable" - {
    //
    //    "using" in {
    //      var isOpen = false
    //      def autoCloseable: Unit = {
    //        isOpen should be(false)
    //        !Arm(new AutoCloseable {
    //          def close(): Unit = {
    //            isOpen should be(true)
    //            isOpen = false
    //          }
    //        })
    //        isOpen should be(true)
    //      }
    //      isOpen should be(false)
    //    }

    "scope" - {

      "arm" in {
        var isOpen = false

        def raii: Scope[ExceptionHandling[Stream[Int]]] = Scope.noop {
          ExceptionHandling.success {
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
        val stream = raii(identity) { e =>
          throw e
        }

        stream should be(Stream(1, 2, 3))
        isOpen should be(false)
      }
    }

  }
}
