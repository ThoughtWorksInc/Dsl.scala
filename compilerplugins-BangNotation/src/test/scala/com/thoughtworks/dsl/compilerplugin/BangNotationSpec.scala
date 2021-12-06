package com.thoughtworks.dsl.compilerplugin

import com.thoughtworks.dsl.Dsl.shift
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** @author 杨博 (Yang Bo)
  */
class BangNotationSpec extends AnyFreeSpec with Matchers {

  "printf problem" in {

    object IntPlaceholder {
      @shift def unary_! : String = ???
      def cpsApply[Domain](f: String => Domain): Int => Domain = { i: Int =>
        f(i.toString)
      }
    }

    object StringPlaceholder {
      @shift def unary_! : String = ???
      def cpsApply[Domain](f: String => Domain): String => Domain = f
    }

    def f1 = "Hello World!"
    def f2 = "Hello " + !StringPlaceholder + "!"
    def f3 = "The value of " + !StringPlaceholder + " is " + !IntPlaceholder + "."

    f1 should be("Hello World!")
    f2.asInstanceOf[String => String]("World") should be("Hello World!")
    f3.asInstanceOf[String => Int => String]("x")(3) should be("The value of x is 3.")
  }

}
