package com.thoughtworks.dsl.macros

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import com.thoughtworks.dsl.macros.Reset.Default.reset

class DottyIssue14510Spec extends AnyFreeSpec with Matchers {
  "https://github.com/lampepfl/dotty/issues/14510" in {
    def compareEnumValue() =
      enum E:
        case V
      reset(E.V == E.V)
    end compareEnumValue

    compareEnumValue() should be(true)


  }
}
