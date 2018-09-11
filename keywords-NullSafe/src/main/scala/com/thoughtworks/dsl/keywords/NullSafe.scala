package com.thoughtworks.dsl.keywords
import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{Keyword, ResetAnnotation, reset, shift}
import com.thoughtworks.dsl.keywords.NullSafe.?
import com.thoughtworks.dsl.keywords.NullSafe.OpaqueTypes.NotNull

import scala.language.implicitConversions
import scala.language.higherKinds
import scala.annotation.{StaticAnnotation, TypeConstraint, compileTimeOnly}

/** [[NullSafe]] is a keyword to perform `null` check.
  *
  * @example You can use [[NullSafe$.? ?]] annotation to represent a nullable value.
  *
  *          {{{
  *          import com.thoughtworks.dsl.keywords.NullSafe._
  *
  *          case class Tree(left: Tree @ $qmark = null, right: Tree @ $qmark = null, value: String @ $qmark = null)
  *
  *          val root: Tree @ $qmark = Tree(
  *            left = Tree(
  *              left = Tree(value = "left-left"),
  *              right = Tree(value = "left-right")
  *            ),
  *            right = Tree(value = "right")
  *          )
  *          }}}
  *
  *          Normal `.` is not null safe, when selecting nullable `left`, `right` or `value`.
  *
  *          {{{
  *          a[NullPointerException] should be thrownBy {
  *            root.right.left.right.value
  *          }
  *          }}}
  *
  *          The above code throws an exception because `root.right.left` is `null`.
  *
  *          The exception can be avoided by using [[?]] on a nullable value:
  *
  *          {{{
  *          root.?.right.?.left.?.value should be(null)
  *          }}}
  *
  *          The entire expression is `null` if one of `?` is performed on a `null` value.
  *
  *          <hr/>
  *
  *          The boundary of a null safe operator [[?]] is the nearest enclosing expression
  *          whose type is annotated as [[NullSafe$.? ?]].
  *
  *          {{{
  *          ("Hello " + ("world " + root.?.right.?.left.?.value)) should be("Hello world null")
  *          ("Hello " + (("world " + root.?.right.?.left.?.value.?): @ $qmark)) should be("Hello null")
  *          (("Hello " + ("world " + root.?.right.?.left.?.value.?)): @ $qmark) should be(null)
  *          }}}
  *
  * @author 杨博 (Yang Bo)
  */
final case class NullSafe[A <: AnyRef](nullable: A @ ?) extends AnyVal {

  @inline
  final def cpsApply[Domain >: Null](handler: NotNull[A] => Domain @ ?): Domain @ ? = {
    if (nullable == null) {
      null
    } else {
      handler(NotNull(nullable))
    }
  }

  @shift
  @compileTimeOnly(
    """This method requires the compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-bangnotation" % "latest.release")` and must only be called inside a code block annotated as `@reset`.""")
  final def ? : NotNull[A] = {
    throw new IllegalAccessException(
      """This method requires the compiler plugin: `addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-bangnotation" % "latest.release")` and must only be called inside a code block annotated as `@reset`."""
    )
  }

}

object NullSafe {

  trait OpaqueTypes {
    type NotNull[A] <: A

    private[NullSafe] def NotNull[A](a: A): NotNull[A]
  }

  val OpaqueTypes: OpaqueTypes = new OpaqueTypes {
    type NotNull[A] = A
    private[NullSafe] def NotNull[A](a: A) = a
  }

  /** @template */
  type ? = reset

  implicit def implicitNullSafe[A <: AnyRef](nullable: A @ ?) = new NullSafe[A](nullable)

}
