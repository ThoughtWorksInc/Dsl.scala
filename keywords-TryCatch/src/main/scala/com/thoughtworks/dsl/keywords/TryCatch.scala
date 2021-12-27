package com.thoughtworks.dsl
package keywords
import Dsl.!!
import Dsl.IsKeyword
import scala.util.control.Exception.Catcher
import scala.concurrent._
import scala.util.control.NonFatal
import com.thoughtworks.dsl.Dsl.cpsApply
import scala.annotation.implicitNotFound

case class TryCatch[+BlockKeyword, +CaseKeyword](
    block: () => BlockKeyword,
    cases: Catcher[CaseKeyword]
) extends Dsl.Keyword.Trait
object TryCatch extends TryCatch.LowPriority0 {
  @implicitNotFound(
    "The `try` ... `catch` expression cannot contain !-notation inside a function that returns ${OuterDomain}."
  )
  opaque type DslComposer[OuterDomain, Value, BlockDomain] <: [
      BlockKeyword,
      CaseKeyword
  ] => (
      Dsl.Searching[BlockKeyword, BlockDomain, Value],
      Dsl.Searching[CaseKeyword, BlockDomain, Value]
  ) ?=> Dsl.Composed[TryCatch[BlockKeyword, CaseKeyword], OuterDomain, Value] =
    [BlockKeyword, CaseKeyword] => (
        Dsl.Searching[BlockKeyword, BlockDomain, Value],
        Dsl.Searching[CaseKeyword, BlockDomain, Value]
    ) ?=> Dsl.Composed[TryCatch[BlockKeyword, CaseKeyword], OuterDomain, Value]
  object DslComposer:
    def apply[OuterDomain, Value, BlockDomain]: (
        [
            BlockKeyword,
            CaseKeyword
        ] => (
            Dsl.Searching[BlockKeyword, BlockDomain, Value],
            Dsl.Searching[CaseKeyword, BlockDomain, Value]
        ) ?=> Dsl.Composed[TryCatch[
          BlockKeyword,
          CaseKeyword
        ], OuterDomain, Value]
    ) =:= DslComposer[OuterDomain, Value, BlockDomain] = summon

  private[TryCatch] trait LowPriority0:
    given [
        State,
        OuterDomain,
        Value,
        BlockDomain
    ](using
        restComposer: DslComposer[OuterDomain, Value, BlockDomain]
    ): DslComposer[State => OuterDomain, Value, State => BlockDomain] =
      DslComposer {
        [BlockKeyword, CaseKeyword] =>
          (
              blockDsl: Dsl.Searching[
                BlockKeyword,
                State => BlockDomain,
                Value
              ],
              caseDsl: Dsl.Searching[CaseKeyword, State => BlockDomain, Value]
          ) ?=>
            Dsl.Composed[TryCatch[
              BlockKeyword,
              CaseKeyword
            ], State => OuterDomain, Value] { (keyword, outerSuccessHandler) =>
              val TryCatch(block, catcher) = keyword
              (state: State) =>
                def withState[Keyword](keyword: Keyword)(using
                    Dsl.Searching[Keyword, State => BlockDomain, Value]
                ): Shift[BlockDomain, Value] = Shift {
                  (blockHandler: (Value => BlockDomain)) =>
                    keyword.cpsApply { (value: Value) => (state: State) =>
                      blockHandler(value)
                    }(state)
                }
                restComposer[Shift[BlockDomain, Value], Shift[
                  BlockDomain,
                  Value
                ]](
                  TryCatch(
                    () => withState[BlockKeyword](block()),
                    { case catcher(caseKeyword) =>
                      withState[CaseKeyword](caseKeyword)
                    }
                  ),
                  outerSuccessHandler(_)(state)
                )
          }
      }

  given [
      LeftDomain,
      Value
  ](using
      fenceDsl: Dsl.Searching[Fence.type, LeftDomain, Unit]
  ): DslComposer[LeftDomain !! Throwable, Value, LeftDomain !! Throwable] =
    DslComposer {
      [BlockKeyword, CaseKeyword] =>
        (
            blockDsl: Dsl.Searching[
              BlockKeyword,
              LeftDomain !! Throwable,
              Value
            ],
            caseDsl: Dsl.Searching[
              CaseKeyword,
              LeftDomain !! Throwable,
              Value
            ]
        ) ?=>
          Dsl.Composed[TryCatch[
            BlockKeyword,
            CaseKeyword
          ], LeftDomain !! Throwable, Value] {
            case (
                  TryCatch(block, catcher),
                  outerSuccessHandler: (Value => LeftDomain !! Throwable)
                ) =>
              outerFailureHandler =>
                def success(a: Value): LeftDomain !! Throwable = {
                  failureHandler =>
                    try {
                      // TODO: Trampoline
                      fenceDsl(
                        Fence,
                        _ => outerSuccessHandler(a)(outerFailureHandler)
                      )
                    } catch {
                      case NonFatal(nativeThrown) =>
                        outerFailureHandler(nativeThrown)
                    }
                }
                def innerFailureHandler(e: Throwable) = {
                  e match {
                    case catcher(recovered) =>
                      caseDsl(recovered, success)(outerFailureHandler)
                    case e =>
                      outerFailureHandler(e)
                  }
                }
                try {
                  fenceDsl(
                    Fence,
                    _ =>
                      blockDsl(
                        block(),
                        success
                      )(innerFailureHandler)
                  )
                } catch {
                  case NonFatal(e) =>
                    innerFailureHandler(e)
                }
        }
    }

  given [BlockValue, OuterValue](using ExecutionContext): DslComposer[
    Future[OuterValue],
    BlockValue,
    Future[BlockValue]
  ] = DslComposer {
    [BlockKeyword, CaseKeyword] =>
      (
          blockDsl: Dsl.Searching[BlockKeyword, Future[
            BlockValue
          ], BlockValue],
          caseDsl: Dsl.Searching[CaseKeyword, Future[BlockValue], BlockValue]
      ) ?=>
        Dsl.Composed[TryCatch[BlockKeyword, CaseKeyword], Future[
          OuterValue
        ], BlockValue] {
          case (
                TryCatch(block, catcher),
                outerSuccessHandler: (BlockValue => Future[OuterValue])
              ) =>
            val blockFuture =
              try {
                summon[Dsl.Run[BlockKeyword, Future[BlockValue], BlockValue]](
                  block()
                )
              } catch {
                case NonFatal(e) =>
                  Future.failed(e)
              }
            blockFuture
              .recoverWith { case catcher(recovered) =>
                summon[
                  Dsl.Run[CaseKeyword, Future[BlockValue], BlockValue]
                ](recovered)
              }
              .flatMap(outerSuccessHandler)
      }
  }

  given [Value, OuterDomain, BlockKeyword, BlockDomain, CaseKeyword](using
      dslTryCatch: TryCatch.DslComposer[OuterDomain, Value, BlockDomain]
  )(using
      Dsl.Searching[BlockKeyword, BlockDomain, Value],
      Dsl.Searching[CaseKeyword, BlockDomain, Value]
  ): Dsl.Composed[TryCatch[BlockKeyword, CaseKeyword], OuterDomain, Value] =
    dslTryCatch[BlockKeyword, CaseKeyword]

}
