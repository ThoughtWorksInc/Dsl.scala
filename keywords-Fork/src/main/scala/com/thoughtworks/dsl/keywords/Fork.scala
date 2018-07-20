package com.thoughtworks.dsl.keywords

import java.io.{PrintStream, PrintWriter}
import java.util.concurrent.atomic.AtomicInteger

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.{!!, Keyword}
import com.thoughtworks.dsl.keywords.Catch.CatchDsl

import scala.collection.{GenTraversableOnce, mutable}
import scala.collection.generic.CanBuildFrom
import scala.util.Try
import scala.language.implicitConversions

final case class Fork[Element](elements: Traversable[Element]) extends AnyVal with Keyword[Fork[Element], Element]

object Fork {
  implicit def implicitFork[Element](elements: Traversable[Element]): Fork[Element] = Fork[Element](elements)

  final case class MultipleException(throwableSet: Set[Throwable])
      extends RuntimeException("Multiple exceptions found") {
    override def toString: String = throwableSet.mkString("\n")

    override def printStackTrace(): Unit = {
      for (throwable <- throwableSet) {
        throwable.printStackTrace()
      }
    }

    override def printStackTrace(s: PrintStream): Unit = {
      for (throwable <- throwableSet) {
        throwable.printStackTrace(s)
      }
    }

    override def printStackTrace(s: PrintWriter): Unit = {
      for (throwable <- throwableSet) {
        throwable.printStackTrace(s)
      }
    }

    override def getStackTrace: Array[StackTraceElement] = synchronized {
      super.getStackTrace match {
        case null =>
          setStackTrace(throwableSet.flatMap(_.getStackTrace)(collection.breakOut))
          super.getStackTrace
        case stackTrace =>
          stackTrace
      }
    }

    override def fillInStackTrace(): this.type = {
      this
    }

  }

  implicit def forkContinuationDsl[NarrowElement, LeftDomain, WidenElement, RightDomain](
      implicit eachDsl: Dsl[Each[NarrowElement], LeftDomain, NarrowElement],
      booleanEachDsl: Dsl[Each[Boolean], LeftDomain, Boolean],
      isTraversableOnce: RightDomain <:< TraversableOnce[WidenElement],
      canBuildFrom: CanBuildFrom[Nothing, WidenElement, RightDomain],
      hangDsl: Dsl[Hang[Unit], LeftDomain, Unit],
      catchDsl: CatchDsl[LeftDomain, LeftDomain, Unit]
  ): Dsl[Fork[NarrowElement], LeftDomain !! RightDomain, NarrowElement] =
    new Dsl[Fork[NarrowElement], LeftDomain !! RightDomain, NarrowElement] {
      def cpsApply(fork: Fork[NarrowElement],
                    mapper: NarrowElement => LeftDomain !! RightDomain): LeftDomain !! RightDomain = _ {
        val builder: mutable.Builder[WidenElement, RightDomain] = canBuildFrom()
        val exceptionBuilder = Set.newBuilder[Throwable]
        val counter = new AtomicInteger(1)
        if (!Each(Seq(true, false))) {
          val element = !Each(fork.elements)
          counter.incrementAndGet()
          try {
            builder ++= !Shift(mapper(element));
            ()
          } catch {
            case MultipleException(throwableSet) =>
              exceptionBuilder ++= throwableSet;
              ()
            case e: Throwable =>
              exceptionBuilder += e;
              ()
          } finally {
            if (counter.decrementAndGet() > 0) {
              !Hang[Unit]()
            }
          }
        } else {
          if (counter.decrementAndGet() > 0) {
            !Hang[Unit]()
          }
        }

        val exceptions = exceptionBuilder.result()
        if (exceptions.isEmpty) {
          builder.result()
        } else {
          val i = exceptions.iterator
          val firstException = i.next()
          if (i.hasNext) {
            throw MultipleException(exceptions)
          } else {
            throw firstException
          }
        }
      }
    }

}
