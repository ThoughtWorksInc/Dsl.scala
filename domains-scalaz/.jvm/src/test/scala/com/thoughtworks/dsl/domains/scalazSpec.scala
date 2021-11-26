package com.thoughtworks.dsl.domains

import com.thoughtworks.dsl.Dsl.!!
import _root_.scalaz.OptionT
import _root_.scalaz.concurrent.Task
import _root_.scalaz.std.stream._
import com.thoughtworks.dsl.domains.scalaz._
import com.thoughtworks.dsl.keywords.{Monadic, Shift, Yield}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** @author 杨博 (Yang Bo)
  */
class scalazSpec extends AnyFreeSpec with Matchers {

  "MonadError" in {
    def task: Task[Int] = Task.now {
      import com.thoughtworks.dsl.keywords.Monadic._
      try {
        0 / 0
      } catch {
        case e: ArithmeticException =>
          42
      } finally {
        !Task.now(())
      }
    }
    task.unsafePerformSync should be(42)
  }

  "Given a continuation that uses Yield and Monadic expressions" - {

    def asyncFunction: Stream[String] !! Unit = _ {
      !Yield("Entering asyncFunction")
      val subThreadId = !Monadic(Stream(0, 1))
      !Yield(s"Fork sub-thread $subThreadId")
      !Yield("Leaving asyncFunction")
    }

    "When create a generator that contains Yield, Shift, and Monadic expressions" - {

      def generator: Stream[String] = {
        !Yield("Entering generator")
        val threadId = !Monadic(Stream(0, 1))
        !Yield(s"Fork thread $threadId")
        !Shift(asyncFunction)
        Stream("Leaving generator")
      }

      "Then the generator should contains yield values" in {
        generator should be(
          Seq(
            /**/ "Entering generator",
            /** *
              */
            "Fork thread 0",
            /** ***
              */
            "Entering asyncFunction",
            /** *****
              */
            "Fork sub-thread 0",
            /** *******
              */
            "Leaving asyncFunction",
            /** *******
              */
            "Leaving generator",
            /** *****
              */
            "Fork sub-thread 1",
            /** *******
              */
            "Leaving asyncFunction",
            /** *******
              */
            "Leaving generator",
            /** *
              */
            "Fork thread 1",
            /** ***
              */
            "Entering asyncFunction",
            /** *****
              */
            "Fork sub-thread 0",
            /** *******
              */
            "Leaving asyncFunction",
            /** *******
              */
            "Leaving generator",
            /** *****
              */
            "Fork sub-thread 1",
            /** *******
              */
            "Leaving asyncFunction",
            /** *******
              */
            "Leaving generator"
          )
        )
      }

    }

  }

  "Given a monadic expression that contains a Scalaz OptionT" - {
    def myOptionalList: OptionT[Stream, String] = {
      // TODO: Is it possible to have `Yield` expressions here?
      val threadId = !Monadic(Stream(0, 1, 2))
      val subThreadId = !Monadic(OptionT(Stream(Some(10), None, Some(30))))
      val subSubThreadId = !Monadic(OptionT(Stream(Some(100), Some(200), None)))
      OptionT[Stream, String](Stream(Some(s"Fork thread $threadId-$subThreadId-$subSubThreadId")))
    }

    "Then it should skips those elements that contains a None" in {
      myOptionalList.run should be(
        Seq(
          Some("Fork thread 0-10-100"),
          Some("Fork thread 0-10-200"),
          None,
          None,
          Some("Fork thread 0-30-100"),
          Some("Fork thread 0-30-200"),
          None,
          Some("Fork thread 1-10-100"),
          Some("Fork thread 1-10-200"),
          None,
          None,
          Some("Fork thread 1-30-100"),
          Some("Fork thread 1-30-200"),
          None,
          Some("Fork thread 2-10-100"),
          Some("Fork thread 2-10-200"),
          None,
          None,
          Some("Fork thread 2-30-100"),
          Some("Fork thread 2-30-200"),
          None
        )
      )
    }

  }
}
