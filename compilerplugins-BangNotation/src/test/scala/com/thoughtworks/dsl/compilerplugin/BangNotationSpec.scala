package com.thoughtworks.dsl.compilerplugin

import com.thoughtworks.dsl.Dsl.reset
import com.thoughtworks.dsl.Dsl.shift
import org.scalatest.{FreeSpec, Matchers}

/**
  * @author 杨博 (Yang Bo)
  */
class BangNotationSpec extends FreeSpec with Matchers {
  import BangNotationSpec._

  "modifying answer type" in {
    def f1 = "Hello World!"
    def f2 = "Hello " + !StringPlaceholder + "!"
    def f3 = "The value of " + !StringPlaceholder + " is " + !IntPlaceholder + "."

    f1 should be("Hello World!")
    f2("World") should be("Hello World!")
    f3("x")(3) should be("The value of x is 3.")
  }

}

object BangNotationSpec {

  trait Placeholder[ParameterType] {
    @shift
    def unary_! : String = {
      throw new RuntimeException("This method should be transformed to `cpsApply`.")
    }

    def cpsApply[Result](f: String => Result): ParameterType => Result = apply(f)
    def apply[Result](f: String => Result): ParameterType => Result
  }

  object IntPlaceholder extends Placeholder[Int] {
    def apply[Result](f: String => Result): Int => Result = { i: Int =>
      f(i.toString)
    }
  }

  object StringPlaceholder extends Placeholder[String] {
    def apply[Result](f: String => Result): String => Result = f
  }

}
