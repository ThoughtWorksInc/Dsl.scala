package com.thoughtworks.dsl.instructions

import com.thoughtworks.dsl.domains.ExceptionHandling
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class ArmSpec extends FreeSpec with Matchers {

  "AutoCloseable" - {

    type Scope[A] = (A => A) => A
    def noop[A](a: A): Scope[A] = _(a)

    "scope" - {

      "arm" in {
        var isOpen = false

        def raii: Scope[ExceptionHandling[Stream[Int]]] = noop {
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
