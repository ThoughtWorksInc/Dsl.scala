package com.thoughtworks.dsl

import scala.annotation.{StaticAnnotation, TypeConstraint}

/** The namespace that contains annotations of control operators for [[https://en.wikipedia.org/wiki/Delimited_continuation delimited continuation]].
  *
  * All annotations defined here requires the Scala compiler plugin, which can be added in `build.sbt`:
  * `<pre>
  * addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugin" % "latest.release")
  * </pre>`
  *
  * == Example of definition of shift control operator ==
  *
  * Given a class that contains a `doShift` method, which is marked as `@shift`
  *
  * {{{
  * import com.thoughtworks.dsl.annotations.{reset, shift}
  * implicit class ShiftOps[R, A](f: (A => R) => R) {
  *   def cpsApply(handler: A => R): R = f(handler)
  *   @shift def doShift: A = sys.error("Calls to this method should have been translated to `cpsApply`")
  * }
  * }}}
  *
  * @example When creating a `@reset` block that contains the `doShift` method,
  *          then the `doShift` method should be translated to `cpsApply` with a handler of captured rest code in the method,
  *
  *          {{{
  *          val explicitResetResult = {
  *            val i: Int = { (handler: Int => Int) =>
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
  *          and the result differs if you move the [[reset]] annotation.
  *
  *          {{{
  *          val explicitResetResult2 = {
  *            val i: Int = { (handler: Int => Int) =>
  *              handler(4) + 2
  *            }.doShift : @reset
  *            i * 10
  *          }
  *
  *          explicitResetResult2 should be {
  *            val i = ShiftOps { (handler: Int => Int) =>
  *              handler(4) + 2
  *            }.cpsApply(identity)
  *            i * 10
  *          }
  *
  *          explicitResetResult2 should be(60)
  *          }}}
  *
  * @note If you omit an explicit [[reset]] annotation,
  *
  *       {{{
  *       def automaticallyReset: Int = {
  *         val i: Int = { (handler: Int => Int) =>
  *           handler(4) + 2
  *         }.doShift
  *         i * 10
  *       }
  *       }}}
  *
  *       then the [[reset]] operation will be performed on the enclosing method or function automatically.
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
  * @see [[Dsl.Instruction]] for a sophisticated implementation of `cpsApply`.
  */
object annotations {

  /** An annotation to explicitly perform reset control operator on a code block. */
  final class reset extends StaticAnnotation

  /** An annotation to mark a method is a shift control operator. */
  final class shift extends StaticAnnotation

}
