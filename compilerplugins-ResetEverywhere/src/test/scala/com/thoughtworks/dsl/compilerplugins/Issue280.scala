package com.thoughtworks.dsl.compilerplugins

object Issue280 {
  import scala.language.experimental.macros

  object ContainsMacro {

    def apply: String = macro Macros.apply

  }

  class Macros(val c: scala.reflect.macros.blackbox.Context) {

    import c.universe._

    def apply: Tree = q"???"

  }

}
