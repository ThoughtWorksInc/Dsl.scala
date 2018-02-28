package com.thoughtworks.dsl.instructions

import java.util.concurrent.atomic.AtomicInteger

import com.thoughtworks.dsl.Dsl
import com.thoughtworks.dsl.Dsl.Instruction

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.util.control.NonFatal

final case class Fork[Element](elements: Traversable[Element]) extends AnyVal with Instruction[Fork[Element], Element]

object Fork {

  implicit def forkContinuationDsl[ThisElement, Domain, ThatElement, That](
      implicit eachDsl: Dsl[Each[ThisElement], Domain, ThisElement],
      booleanEachDsl: Dsl[Each[Boolean], Domain, Boolean],
      isTraversableOnce: That <:< TraversableOnce[ThatElement],
      canBuildFrom: CanBuildFrom[Nothing, ThatElement, That],
      hangDsl: Dsl[Hang[Unit], Domain, Unit],
      catchDsl: Dsl[com.thoughtworks.dsl.instructions.Catch[Domain], Domain, Domain => Domain]
  ): Dsl[Fork[ThisElement], (That => Domain) => Domain, ThisElement] =
    new Dsl[Fork[ThisElement], (That => Domain) => Domain, ThisElement] {
      def interpret(fork: Fork[ThisElement],
                    mapper: ThisElement => (That => Domain) => Domain): (That => Domain) => Domain = _ {
        val builder = canBuildFrom()
        val exceptionBuilder = Set.newBuilder[Throwable]
        val counter = new AtomicInteger(1)
        if (!Each(Seq(true, false))) {
          val element = !Each(fork.elements)
          counter.incrementAndGet()
          try {
            builder ++= !Shift(mapper(element))
          } catch {
            case NonFatal(e) =>
              exceptionBuilder += e
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
        builder.result()
      }
    }

}
