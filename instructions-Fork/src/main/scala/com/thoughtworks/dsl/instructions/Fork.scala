package com.thoughtworks.dsl.instructions

import java.io.{PrintStream, PrintWriter}
import java.util.concurrent.atomic.AtomicInteger

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.collection.{GenTraversableOnce, mutable}
import scala.collection.generic.CanBuildFrom
import scala.util.Try
import scala.util.control.NonFatal

final case class Fork[Element](elements: Traversable[Element]) extends AnyVal with Instruction[Fork[Element], Element]

object Fork {

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

  implicit def forkContinuationDsl[ThisElement, Domain, ThatElement, That](
      implicit eachDsl: Dsl[Each[ThisElement], Domain, ThisElement],
      booleanEachDsl: Dsl[Each[Boolean], Domain, Boolean],
      isTraversableOnce: That <:< TraversableOnce[ThatElement],
      canBuildFrom: CanBuildFrom[Nothing, ThatElement, That],
      hangDsl: Dsl[Hang[Unit], Domain, Unit],
      scopeDsl: Dsl[Scope[Domain, Try[Unit]], Domain, Try[Unit]],
      catchDsl: Dsl[Catch[Domain], Domain, Unit]
  ): Dsl[Fork[ThisElement], (That => Domain) => Domain, ThisElement] =
    new Dsl[Fork[ThisElement], (That => Domain) => Domain, ThisElement] {
      def interpret(fork: Fork[ThisElement],
                    mapper: ThisElement => (That => Domain) => Domain): (That => Domain) => Domain = _ {
        val builder: mutable.Builder[ThatElement, That] = canBuildFrom()
        val exceptionBuilder = Set.newBuilder[Throwable]
        val counter = new AtomicInteger(1)
        if (!Each(Seq(true, false))) {
          val element = !Each(fork.elements)
          counter.incrementAndGet()
          try {
            builder ++= !Shift(mapper(element))
            ()
          } catch {
            case e: Throwable =>
              exceptionBuilder += e
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
