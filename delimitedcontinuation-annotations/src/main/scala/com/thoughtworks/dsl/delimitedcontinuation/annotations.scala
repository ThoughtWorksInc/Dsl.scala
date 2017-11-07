package com.thoughtworks.dsl.delimitedcontinuation

import scala.annotation.{StaticAnnotation, TypeConstraint}

/** The namespace that contains annotations of control operators for [[https://en.wikipedia.org/wiki/Delimited_continuation delimited continuation]].
  *
  * All annotations defined here requires the Scala compiler plugin, which can be added in `build.sbt`:
  * `<pre>
  * addCompilerPlugin("com.thoughtworks.dsl" %% "delimitedcontinuation-compilerplugin" % "latest.release")
  * </pre>`
  *
  * == Example of definition of shift control operator ==
  *
  * Given a class that contains a `doShift` method, which is marked as `@shift`
  *
  * {{{
  * import com.thoughtworks.dsl.delimitedcontinuation.annotations.{reset, shift}
  * case class ShiftOps[R, A](f: (A => R) => R) {
  *   def cpsApply(handler: A => R): R = f(handler)
  *   @shift def doShift: A = ???
  * }
  * }}}
  *
  * @example When creating a `@reset` block that contains the `doShift` method.
  *          Then the `doShift` method should be translated to `cpsApply` with a handler of captured rest code in the method
  *
  *          {{{
  *          val explicitResetResult = {
  *            val i: Int = ShiftOps { (handler: Int => Int) =>
  *              handler(4) + 2
  *            }.doShift
  *            i * 10
  *          }: @reset
  *
  *          explicitResetResult should be {
  *            ShiftOps { (handler: Int => Int) =>
  *              handler(4) + 2
  *            }.cpsApply { i =>
  *              i * 10
  *            }
  *          }
  *
  *          explicitResetResult should be(42)
  *          }}}
  *
  * @note If you omit an explicit [[reset]] annotation,
  *
  *       {{{
  *       def automaticallyReset: Int = {
  *         val i: Int = ShiftOps { (handler: Int => Int) =>
  *           handler(4) + 2
  *         }.doShift
  *         i * 10
  *       }
  *       }}}
  *
  *       then the reset operation will be performed on the enclosing method or function automatically.
  *
  *       {{{
  *       automaticallyReset should be {
  *         ShiftOps { (handler: Int => Int) =>
  *           handler(4) + 2
  *         }.cpsApply { i =>
  *           i * 10
  *         }
  *       }
  *
  *       automaticallyReset should be(42)
  *       }}}
  */
object annotations {

  /** An annotation to explicitly perform reset control operator on a code block. */
  final class reset extends StaticAnnotation with TypeConstraint

  /** An annotation to mark a method is a shift control operator. */
  final class shift extends StaticAnnotation

}
