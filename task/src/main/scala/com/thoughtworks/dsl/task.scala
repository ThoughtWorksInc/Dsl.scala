package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.reset
import com.thoughtworks.dsl.domains.ExceptionHandling
import com.thoughtworks.dsl.instructions.{Each, Shift}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Future, Promise}
import scala.language.implicitConversions

/**
  * @author 杨博 (Yang Bo)
  */
object task {

  trait Trampoline1[A, R] extends Function1[A, R] {
    def step(): A => R

    @tailrec
    final def apply(a: A): R = {
      step() match {
        case trampoline: Trampoline1[A, R] =>
          trampoline(a)
        case last =>
          last(a)
      }
    }
  }

  type Task[+A] = (A => ExceptionHandling[Unit]) => ExceptionHandling[Unit]

  object Task {

    def join[Element, That](element: Element)(
        implicit canBuildFrom: CanBuildFrom[Nothing, Element, That]): Task[That] @reset = now {
      (canBuildFrom() += element).result()
    }

    def suspend[A](stepTask: () => Task[A]): Task[A] = {
      new Trampoline1[(A => ExceptionHandling[Unit]), ExceptionHandling[Unit]] {
        def step(): (A => ExceptionHandling[Unit]) => ExceptionHandling[Unit] = stepTask()
      }
    }

    @inline
    def now[A](a: A): Task[A] = _(a)

    @inline
    def delay[A](f: () => A): Task[A] = _(f())

    @inline
    def reset[A](a: A): Task[A] @reset = Task.now(a)

  }

  implicit def await[Domain, Value](continuation: (Value => Domain) => Domain): Shift[Domain, Value] =
    Shift(continuation)

  implicit def reset[A](a: A): Task[A] @reset = Task.now(a)

  def taskToFuture[A](task: Task[A]): Future[A] = {
    val promise = Promise[A]()
    task { a: A =>
      promise.success(a)
      ExceptionHandling.success(())
    } { e: Throwable =>
      promise.failure(e)
    }
    promise.future
  }

}
