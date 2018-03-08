package com.thoughtworks.dsl.keywords

import com.thoughtworks.dsl.Dsl.Keyword

import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
final case class AutoClose[R <: AutoCloseable](open: () => R) extends AnyVal with Keyword[AutoClose[R], R]

object AutoClose {

  implicit def implicitAutoClose[R <: AutoCloseable](r: => R): AutoClose[R] = AutoClose[R](r _)

  def apply[R <: AutoCloseable](r: => R)(
      implicit dummyImplicit: DummyImplicit = DummyImplicit.dummyImplicit): AutoClose[R] = new AutoClose(r _)

}
