package com.thoughtworks.dsl
package keywords
import com.thoughtworks.dsl.Dsl.IsKeyword

/** [[Put]] is a [[Dsl.Keyword Keyword]] to replace the [[value]] of the current
  * context.
  *
  * Purely functional programming languages usually do not support native
  * first-class mutable variables. In those languages, mutable states can be
  * implemented in state monads.
  *
  * [[Put]] and [[Get]] are the [[Dsl]]-based replacements of state monads.
  *
  * We use unary function as the domain of mutable state. The parameter of the
  * unary function can be read from the [[Get]] keyword, and changed by the
  * [[Put]] keyword.
  *
  * @example
  *   The following example creates a function that accepts a string parameter
  *   and returns the upper-cased last character of the parameter.
  *
  * {{{
  * import com.thoughtworks.dsl.macros.Reset.Default.reset
  * def upperCasedLastCharacter = reset[String => Char] {
  *   val initialValue = !Get[String]
  *   !Put(initialValue.toUpperCase)
  *
  *   val upperCased = !Get[String]
  *   Function.const(upperCased.last)
  * }
  * }}}
  *
  * For example, given a string of `foo`, the upper-cased last character should
  * be `O`.
  *
  * {{{
  * upperCasedLastCharacter("foo") should be('O')
  * }}}
  *
  * @example
  *   [[Put]] and [[Get]] support multiple states.
  *
  * The following code creates a formatter that [[Put]] parts of content into a
  * `Vector[Any]` of string buffers.
  *
  * {{{
  * import com.thoughtworks.dsl.macros.Reset.Default.reset
  * def formatter = reset[Double => Int => Vector[Any] => String] {
  *   !Put(!Get[Vector[Any]] :+ "x=")
  *   !Put(!Get[Vector[Any]] :+ !Get[Double])
  *   !Put(!Get[Vector[Any]] :+ ",y=")
  *   !Put(!Get[Vector[Any]] :+ !Get[Int])
  *
  *   !Return((!Get[Vector[Any]]).mkString)
  * }
  *
  * formatter(0.5)(42)(Vector.empty) should be("x=0.5,y=42")
  * }}}
  * @see
  *   [[Get]]
  * @author
  *   杨博 (Yang Bo)
  */
opaque type Put[+S] <: Dsl.Keyword.Opaque = Dsl.Keyword.Opaque.Of[S]
def Put[S](using
    dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit
): S =:= Put[S] = Dsl.Keyword.Opaque.Of

object Put {
  given [S]: IsKeyword[Put[S], Unit] with {}

  given [S0, S >: S0, A]: Dsl.Original[Put[S0], S => A, Unit] =
    Dsl.Original[Put[S0], S => A, Unit] {
      (keyword: Put[S0], handler: Unit => S => A) => oldValue =>
        handler(())(keyword)
    }
}
