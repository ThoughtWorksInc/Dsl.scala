package com.thoughtworks.dsl

import com.thoughtworks.dsl.Dsl.{Trampoline1, reset}
import com.thoughtworks.dsl.domains.Raii
import com.thoughtworks.dsl.instructions.{Each, Shift}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.concurrent.{Future, Promise}
import scala.language.implicitConversions
import scala.util.control.NonFatal

/**
  * @author 杨博 (Yang Bo)
  */
object task {

  type Task[+A] = (A => Raii[Unit]) => Raii[Unit]

  implicit final class TaskOps[+A](task: Task[A]) {
    def onComplete(successHandler: A => Unit, failureHandler: Throwable => Unit): Unit = {
      (try {
        task { a =>
          successHandler(a)
          Raii.success()
        }
      } catch {
        case e: Throwable =>
          return failureHandler(e)
      }).onFailure(failureHandler)
    }
  }

  object Task {

    def join[Element, That](element: Element)(
        implicit canBuildFrom: CanBuildFrom[Nothing, Element, That]): Task[That] @reset = now {
      (canBuildFrom() += element).result()
    }

    def suspend[A](stepTask: () => Task[A]): Task[A] = {
      new Trampoline1[(A => Raii[Unit]), Raii[Unit]] {
        def step(): (A => Raii[Unit]) => Raii[Unit] = stepTask()
      }
    }

    @inline
    def now[A](a: A): Task[A] = _(a)

    @inline
    def delay[A](f: () => A): Task[A] = { continue =>
      continue(f())
    }

    @inline
    implicit def reset[A](a: => A): Task[A] @reset = delay(a _)

  }

  implicit def await[Domain, Value](continuation: (Value => Domain) => Domain): Shift[Domain, Value] =
    Shift(continuation)

  def taskToFuture[A](task: Task[A]): Future[A] = {
    val promise = Promise[A]()
    task { a: A =>
      promise.success(a)
      Raii.success(())
    }.onFailure { e: Throwable =>
      promise.failure(e)
    }
    promise.future
  }

}
