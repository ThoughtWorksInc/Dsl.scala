package com.thoughtworks
package dsl
package macros
import com.thoughtworks.dsl.keywords._, Match._
import scala.quoted.Quotes
import collection.immutable.Queue
import scala.util.control.Exception.Catcher

/** Macros to translate the [[reset]] operator of a delimited continuation,
  * where all the control flows are an AST of [[keywords]], interpreted by the
  * [[Dsl]] type class.
  *
  * @example
  *   Suppose you are generating a random integer less than 100, whose first
  *   digit and second digit is different. A solution is generating integers in
  *   an infinite loop, and [[Return]] from the loop when the generated integer
  *   conforms with requirements.
  *
  * {{{
  * import scala.util.Random
  * import scala.util.control.TailCalls
  * import scala.util.control.TailCalls.TailRec
  * import com.thoughtworks.dsl.macros.Reset.Default.reset
  * def randomInt(): TailRec[Int] = reset {
  *   while (true) {
  *     val r = Random.nextInt(100)
  *     if (r % 10 != r / 10) {
  *       !Return(TailCalls.done(r))
  *     }
  *   }
  *   throw new AssertionError("Unreachable code");
  * }
  *
  * val r = randomInt().result
  * r should be < 100
  * r % 10 should not be r / 10
  * }}}
  *
  * @example
  *   Since the [[Return]] keyword can automatically lift the return type,
  *   `TailCalls.done` can be omitted.
  *
  * {{{
  * import scala.util.Random
  * import scala.util.control.TailCalls
  * import scala.util.control.TailCalls.TailRec
  * import com.thoughtworks.dsl.macros.Reset.Default.reset
  * def randomInt(): TailRec[Int] = reset {
  *   while (true) {
  *     val r = Random.nextInt(100)
  *     if (r % 10 != r / 10) {
  *       !Return(r)
  *     }
  *   }
  *   throw new AssertionError("Unreachable code");
  * }
  *
  * val r = randomInt().result
  * r should be < 100
  * r % 10 should not be r / 10
  * }}}
  */
trait Reset:
  type ShouldResetNestedFunctions <: Boolean & Singleton
  type DontSuspend <: Boolean & Singleton

  transparent inline def reify[Value](inline value: Value): Any = ${
    Reset.Macros.reify[ShouldResetNestedFunctions, DontSuspend, Value]('value)
  }

  class *[Functor[_]]() {
    inline def apply[Value](inline value: Value): Functor[Value] = ${
      Reset.Macros
        .reset[ShouldResetNestedFunctions, DontSuspend, Value, Functor[Value]](
          'value
        )
    }
  }
  inline def *[Domain[_]]: *[Domain] = new *[Domain]

  inline def reset[Value](inline value: Value): Value = ${
    Reset.Macros.reset[ShouldResetNestedFunctions, DontSuspend, Value, Value](
      'value
    )
  }

object Reset {

  /** A [[Reset]] translator with default options */
  val Default = new Reset:
    type ShouldResetNestedFunctions = false
    type DontSuspend = false

  private class Macros[Q <: Quotes](
      shouldResetNestedFunctions: Boolean,
      dontSuspend: Boolean
  )(using val qctx: Q) {
    import qctx.reflect.{_, given}

    def reify[V](
        body: quoted.Expr[_]
    )(using valueType: quoted.Type[V]): quoted.Expr[_] = {
      val bodyTerm = body.asTerm.underlyingArgument
      val reifiedTerm = if (dontSuspend) {
        KeywordTree(bodyTerm).keywordTerm
      } else {
        Suspend(KeywordTree(bodyTerm)).keywordTerm
      }
      reifiedTerm.usingExpr {
        [K] =>
          (k: quoted.Expr[K]) =>
            (tk: quoted.Type[K]) ?=>
              '{ keywords.Typed[K, V]($k) }: quoted.Expr[_]
      }
    }

    def resetDefDef(defDef: DefDef): DefDef = {
      val DefDef(name, typeParamsAndParams, tpt, rhsOption) = defDef
      tpt.tpe.asType match {
        case '[valueType] =>
          rhsOption match {
            case Some(rhs) if shouldResetNestedFunctions =>
              rhs match {
                case matchTree @ qctx.reflect.Match(scrutinee, cases) =>
                  DefDef.copy(defDef)(
                    name,
                    typeParamsAndParams,
                    tpt,
                    Some(
                      qctx.reflect.Match.copy(matchTree)(
                        scrutinee,
                        cases.map {
                          case caseDef @ CaseDef(pattern, guard, caseRhs) =>
                            CaseDef.copy(caseDef)(
                              pattern,
                              guard,
                              reset[valueType, valueType](
                                caseRhs.asExprOf[valueType]
                              ).asTerm
                            )
                        }
                      )
                    )
                  )
                case _ =>
                  DefDef.copy(defDef)(
                    name,
                    typeParamsAndParams,
                    tpt,
                    Some(
                      reset[valueType, valueType](
                        rhs.asExprOf[valueType]
                      ).asTerm
                    )
                  )
              }
            case _ =>
              defDef
          }
      }
    }

    def reset[Value, Domain](body: quoted.Expr[Value])(using
        valueType: quoted.Type[Value],
        domainType: quoted.Type[Domain]
    ): quoted.Expr[Domain] = {
      KeywordTree(body.asTerm.underlyingArgument) match {
        case Pure(pure, _) if dontSuspend && TypeRepr.of[Value] <:< TypeRepr.of[Domain] =>
          pure.asExprOf[Domain]
        case keywordTree =>
          val reifiedTerm = if (dontSuspend) {
            keywordTree.keywordTerm
          } else {
            Suspend(keywordTree).keywordTerm
          }
          reifiedTerm.usingExpr { [K] => (k: quoted.Expr[K]) => ( keywordType: quoted.Type[K]) ?=>
            {

              Implicits.search(TypeRepr.of[Dsl.Run[K, Domain, Value]]) match {
                case success: ImplicitSearchSuccess =>
                  '{
                    ${success.tree.asExprOf[Dsl.Run[K, Domain, Value]]}.apply($k)
                  }
              }
          }
      }
    }

    extension (tpe: TypeRepr)
      def usingType[B, Bound](
          f: [A <: Bound] => (ta: quoted.Type[A]) => B
      ): B = {
        f( /*given*/ tpe.asType.asInstanceOf)
      }

    extension (term: Term)
      def usingExpr[B, UpperBound](
          f: [A <: UpperBound] => quoted.Expr[A] => (ta: quoted.Type[A]) ?=> B
      ): B = {
        def helper[A <: UpperBound] = {
          implicit val tpe: quoted.Type[A] =
            term.tpe.asType.asInstanceOf[quoted.Type[A]]
          f[A](term.asExprOf[A])(using tpe)
        }
        helper
      }

    val List(shiftSymbol) =
      Symbol.requiredModule("com.thoughtworks.dsl.Dsl").declaredMethod("shift")

    sealed trait KeywordTree {
      def keywordTerm: Term
      def valueType: TypeRepr

      def where(
          blockTemplate: qctx.reflect.Block,
          statement: Statement
      ): KeywordTree = Let(blockTemplate, statement, this)
      def block: KeywordTree = Block(this)
      def usingKeyword[B, Bound](
          f: [K, V <: Bound] => quoted.Expr[K] => (
              tk: quoted.Type[K],
              tv: quoted.Type[V]
          ) => B
      ): B = {
        keywordTerm.usingExpr[B, Any] {
          [K] =>
            (keyword: quoted.Expr[K]) =>
              (keywordType: quoted.Type[K]) ?=>
                valueType.usingType[B, Bound] {
                  [V <: Bound] =>
                    (valueType: quoted.Type[V]) =>
                      given quoted.Type[V] = valueType
                      f(keyword)( /*given*/ keywordType, valueType)
              }
        }
      }
      def flatMap(flatMapper: Term => KeywordTree): KeywordTree = {
        FlatMap(this, flatMapper)
      }
    }

    object KeywordTree {
      def apply(term: Term): KeywordTree = {
        term match {
          case Apply(
                TypeApply(
                  shiftMethod,
                  List(_, valueTypeTree)
                ),
                List(
                  from
                )
              ) if shiftMethod.symbol == shiftSymbol =>
            KeywordTree(from).flatMap { pureFrom =>
              Keyword(pureFrom, valueTypeTree.tpe)
            }
          case typeApply @ TypeApply(fun, args) =>
            KeywordTree(fun).flatMap { pureFun =>
              Pure(TypeApply.copy(typeApply)(pureFun, args), term.tpe)
            }
          case Apply(fun, args) =>
            KeywordTree(fun).flatMap { pureFun =>
              def loop(args: List[Term], pureArgs: Queue[Term]): KeywordTree = {
                args match {
                  case Nil =>
                    Pure(Apply.copy(fun)(pureFun, pureArgs.toList), term.tpe)
                  case head :: tail =>
                    KeywordTree(head).flatMap { pureHead =>
                      loop(tail, pureArgs :+ pureHead)
                    }
                }
              }
              loop(args, Queue.empty)
            }
          case repeated @ Repeated(elems, elemtpt) =>
            def loop(args: List[Term], pureElems: Queue[Term]): KeywordTree = {
              args match {
                case Nil =>
                  Pure(Repeated(pureElems.toList, elemtpt), term.tpe)
                case head :: tail =>
                  KeywordTree(head).flatMap { pureHead =>
                    loop(tail, pureElems :+ pureHead)
                  }
              }
            }
            loop(elems, Queue.empty)
          case select @ Select(qualifier, name) =>
            KeywordTree(qualifier).flatMap { pureQualifier =>
              Pure(Select.copy(select)(pureQualifier, name), term.tpe)
            }
          case selectOuter @ SelectOuter(qualifier, levels, tpe) =>
            KeywordTree(qualifier).flatMap { pureQualifier =>
              Pure(
                SelectOuter.copy(selectOuter)(pureQualifier, levels, tpe),
                term.tpe
              )
            }
          case (_: Literal) | (_: Ident) | (_: New) | (_: This) | (_: Super) |
              (_: Closure) =>
            Pure(term, term.tpe)
          case block @ qctx.reflect.Block(
                List(
                  defDef: DefDef
                ),
                closure @ Closure(ident: Ident, _)
              ) if (ident.name == defDef.name) =>
            Pure(
              qctx.reflect.Block.copy(block)(
                List(resetDefDef(defDef)),
                closure
              ),
              term.tpe
            )
          case block @ qctx.reflect.Block(stats, expr) =>
            def loop(stats: List[Statement] = stats): KeywordTree = {
              stats match {
                case Nil =>
                  KeywordTree(expr)
                case head :: tail =>
                  head match {
                    case headTerm: Term =>
                      KeywordTree(headTerm).flatMap(loop(tail).where(block, _))
                    case headVal @ ValDef(name, tpt, Some(rhs)) =>
                      KeywordTree(rhs).flatMap { pureRhs =>
                        loop(tail).where(
                          block,
                          ValDef.copy(headVal)(name, tpt, Some(pureRhs))
                        )
                      }
                    case defDef: DefDef =>
                      loop(tail).where(block, resetDefDef(defDef))
                    case _ =>
                      loop(tail).where(block, head)
                  }
              }
            }
            loop().block
          case qctx.reflect.If(cond, thenp, elsep) =>
            If(
              KeywordTree(cond),
              KeywordTree(thenp),
              KeywordTree(elsep),
              term.tpe
            )
          case qctx.reflect.Match(upstream, cases) =>
            Match(
              KeywordTree(upstream),
              cases.map { case caseDef @ CaseDef(pattern, guard, body) =>
                caseDef -> KeywordTree(body)
              },
              term.tpe
            )
          case Try(expr, cases, None) =>
            TryCatch(
              KeywordTree(expr),
              cases.map { case caseDef @ CaseDef(pattern, guard, body) =>
                caseDef -> KeywordTree(body)
              },
              term.tpe
            )
          case Try(expr, Nil, Some(finalizer)) =>
            TryFinally(
              KeywordTree(expr),
              KeywordTree(finalizer),
              term.tpe
            )
          case Try(expr, cases, Some(finalizer)) =>
            TryCatchFinally(
              KeywordTree(expr),
              cases.map { case caseDef @ CaseDef(pattern, guard, body) =>
                caseDef -> KeywordTree(body)
              },
              KeywordTree(finalizer),
              term.tpe
            )
          case typed @ qctx.reflect.Typed(expr, tpt) =>
            KeywordTree(expr).flatMap { pureExpr =>
              Pure(qctx.reflect.Typed.copy(typed)(pureExpr, tpt), term.tpe)
            }
          case assign @ Assign(lhs, rhs) =>
            KeywordTree(lhs).flatMap { pureLhs =>
              KeywordTree(rhs).flatMap { pureRhs =>
                Pure(Assign.copy(assign)(pureLhs, pureRhs), term.tpe)
              }
            }
          case Inlined(_, bindings, term) =>
            KeywordTree(qctx.reflect.Block(bindings, term))
          case whileTerm @ qctx.reflect.While(cond, body) =>
            While(KeywordTree(cond), KeywordTree(body))
          // case returnTree @ qctx.reflect.Return(expr, _from) =>
          //   KeywordTree(expr).flatMap { pureExpr =>
          //     Return(pureExpr, expr.tpe)
          //   }
          case returnTree @ qctx.reflect.Return(expr, from) =>
            KeywordTree(expr).flatMap { pureExpr =>
              Pure(
                qctx.reflect.Return.copy(returnTree)(pureExpr, from),
                term.tpe
              )
            }
        }
      }

    }

    private def usingCases[R, A](
        cases: Seq[(CaseDef, KeywordTree)],
        caseValueType: TypeRepr
    )(
        f: [Set, Value] => quoted.Expr[PartialFunction[A, Set]] => (
            ts: quoted.Type[Set],
            tv: quoted.Type[Value]
        ) => R
    )(using
        quoted.Type[A]
    ): R = {
      // val keywordTerms = cases.map { (caseDef, keywordTree) =>
      //   caseDef -> keywordTree.keywordTerm
      // }
      val caseSetType =
        cases.view.zipWithIndex.foldRight(TypeRepr.of[Nothing]) {
          case (((caseDef, keywordTree), i), accumulator) =>
            val indexType = quoted.Expr(i).asTerm.tpe
            val AppliedType(withIndex, _) = TypeRepr.of[WithIndex[_, _]]
            val AppliedType(ccons, _) = TypeRepr.of[Any +: Nothing]
            ccons.appliedTo(
              List(
                withIndex.appliedTo(
                  List(indexType, keywordTree.keywordTerm.tpe)
                ),
                accumulator
              )
            )
        }
      caseValueType.usingType {
        [Value] =>
          (valueTpe: quoted.Type[Value]) =>
            given quoted.Type[Value] = valueTpe
            caseSetType.usingType {
              [Set] =>
                (setTpe: quoted.Type[Set]) =>
                  given quoted.Type[Set] = setTpe
                  '{
                    {
                      case e: Throwable if false => ???
                    }: PartialFunction[A, Set]
                  }.asTerm.underlyingArgument match {
                    case qctx.reflect.Typed(
                          pfBlock @ qctx.reflect.Block(
                            List(
                              defDef @ DefDef(
                                name,
                                typeParamsAndParams,
                                returnTpt,
                                Some(
                                  matchTree @ qctx.reflect.Match(
                                    scrutinee,
                                    List(caseDefTemplate)
                                  )
                                )
                              )
                            ),
                            closure
                          ),
                          _
                        ) =>
                      val newCases = cases.view.zipWithIndex.map {
                        case ((caseDef, caseKeywordTree), i) =>
                          caseKeywordTree.usingKeyword {
                            [BodyKeyword, BodyValue] =>
                              (bodyExpr: quoted.Expr[BodyKeyword]) =>
                                (
                                    bodyKeywordTpe: quoted.Type[BodyKeyword],
                                    bodyValueTpe: quoted.Type[BodyValue]
                                ) =>
                                  given quoted.Type[BodyKeyword] =
                                    bodyKeywordTpe
                                  given quoted.Type[BodyValue] = bodyValueTpe
                                  quoted
                                    .Expr(i)
                                    .asTerm
                                    .usingExpr[CaseDef, Int] {
                                      [Index <: Int] =>
                                        (indexExpr: quoted.Expr[Index]) =>
                                          (indexType: quoted.Type[Index]) ?=>
                                            CaseDef
                                              .copy(caseDef)(
                                                caseDef.pattern,
                                                caseDef.guard,
                                                '{
                                                  WithIndex[Index, BodyKeyword](
                                                    $indexExpr,
                                                    $bodyExpr
                                                  ).asInstanceOf[Set]
                                                }.asTerm
                                              )
                                              .changeOwner(defDef.symbol)
                                  }
                          }
                      }.toList
                      val pfTerm = qctx.reflect.Block.copy(pfBlock)(
                        List(
                          DefDef.copy(defDef)(
                            name,
                            typeParamsAndParams,
                            returnTpt,
                            Some(
                              qctx.reflect.Match
                                .copy(matchTree)(scrutinee, newCases)
                            )
                          )
                        ),
                        closure
                      )
                      f[Set, Value](pfTerm.asExprOf[PartialFunction[A, Set]])(
                        setTpe,
                        valueTpe
                      )
                }
          }
      }
    }

    case class If(
        cond: KeywordTree,
        thenp: KeywordTree,
        elsep: KeywordTree,
        valueType: TypeRepr
    ) extends KeywordTree {
      lazy val keywordTerm = cond.usingKeyword[Term, Boolean] {
        [CondKeyword, CondValue <: Boolean] =>
          (condExpr: quoted.Expr[CondKeyword]) =>
            (
                condKeywordType: quoted.Type[CondKeyword],
                condValueType: quoted.Type[CondValue]
            ) =>
              given quoted.Type[CondKeyword] = condKeywordType
              given quoted.Type[CondValue] = condValueType
              thenp.usingKeyword[Term, Boolean] {
                [ThenpKeyword, ThenpValue <: Boolean] =>
                  (thenpExpr: quoted.Expr[ThenpKeyword]) =>
                    (
                        thenpKeywordType: quoted.Type[ThenpKeyword],
                        thenpValueType: quoted.Type[ThenpValue]
                    ) =>
                      given quoted.Type[ThenpKeyword] = thenpKeywordType
                      given quoted.Type[ThenpValue] = thenpValueType
                      elsep.usingKeyword[Term, Boolean] {
                        [ElsepKeyword, ElsepValue <: Boolean] =>
                          (elsepExpr: quoted.Expr[ElsepKeyword]) =>
                            (
                                elsepKeywordType: quoted.Type[ElsepKeyword],
                                elsepValueType: quoted.Type[ElsepValue]
                            ) =>
                              given quoted.Type[ElsepKeyword] = elsepKeywordType
                              given quoted.Type[ElsepValue] = elsepValueType
                              valueType.usingType {
                                [Result] =>
                                  (resultType: quoted.Type[Result]) =>
                                    given quoted.Type[Result] = resultType
                                    '{
                                      com.thoughtworks.dsl.keywords.If(
                                        $condExpr,
                                        () =>
                                          ${
                                            thenpExpr.asTerm
                                              .changeOwner(Symbol.spliceOwner)
                                              .asExprOf[ThenpKeyword]
                                          },
                                        () =>
                                          ${
                                            elsepExpr.asTerm
                                              .changeOwner(Symbol.spliceOwner)
                                              .asExprOf[ElsepKeyword]
                                          }
                                      )
                                    }.asTerm
                            }
                    }
            }
      }
    }

    case class Match(
        expr: KeywordTree,
        cases: Seq[(CaseDef, KeywordTree)],
        valueType: TypeRepr
    ) extends KeywordTree {
      lazy val keywordTerm = {
        expr.usingKeyword {
          [K, V] =>
            (exprKeywordExpr: quoted.Expr[K]) =>
              (
                  exprKeywordType: quoted.Type[K],
                  exprValueType: quoted.Type[V]
              ) =>
                given quoted.Type[K] = exprKeywordType
                given quoted.Type[V] = exprValueType
                usingCases[Term, V](cases, valueType) {
                  [CaseSet, CaseValue] =>
                    (pf: quoted.Expr[PartialFunction[V, CaseSet]]) =>
                      (
                          caseSetType: quoted.Type[CaseSet],
                          caseValueType: quoted.Type[CaseValue]
                      ) =>
                        given quoted.Type[CaseSet] = caseSetType
                        given quoted.Type[CaseValue] = caseValueType
                        '{
                          com.thoughtworks.dsl.keywords.Match(
                            $exprKeywordExpr,
                            $pf
                          )
                        }.asTerm
              }
        }
      }
    }

    case class TryCatchFinally(
        expr: KeywordTree,
        cases: Seq[(CaseDef, KeywordTree)],
        finalizer: KeywordTree,
        valueType: TypeRepr
    ) extends KeywordTree {
      lazy val keywordTerm = {
        usingCases[Term, Throwable](cases, valueType) {
          [Set, Value] =>
            (pf: quoted.Expr[PartialFunction[Throwable, Set]]) =>
              (setType: quoted.Type[Set], valueType: quoted.Type[Value]) =>
                given quoted.Type[Set] = setType
                given quoted.Type[Value] = valueType
                expr.usingKeyword[Term, Value] {
                  [TryKeyword, TryValue <: Value] =>
                    (tryKeywordExpr: quoted.Expr[TryKeyword]) =>
                      (
                          tryKeywordTpe: quoted.Type[TryKeyword],
                          tryValueTpe: quoted.Type[TryValue]
                      ) =>
                        given quoted.Type[TryKeyword] = tryKeywordTpe
                        given quoted.Type[TryValue] = tryValueTpe
                        finalizer.usingKeyword {
                          [FinalizerKeyword, FinalizerValue] =>
                            (finalizerKeywordExpr: quoted.Expr[
                              FinalizerKeyword
                            ]) =>
                              (
                                  finalizerKeywordType: quoted.Type[
                                    FinalizerKeyword
                                  ],
                                  finalizerValueType: quoted.Type[
                                    FinalizerValue
                                  ]
                              ) =>
                                given quoted.Type[FinalizerKeyword] =
                                  finalizerKeywordType
                                given quoted.Type[FinalizerValue] =
                                  finalizerValueType
                                '{
                                  com.thoughtworks.dsl.keywords.TryCatchFinally(
                                    () =>
                                      ${
                                        tryKeywordExpr.asTerm
                                          .changeOwner(Symbol.spliceOwner)
                                          .asExprOf[TryKeyword]
                                      },
                                    $pf,
                                    () =>
                                      ${
                                        finalizerKeywordExpr.asTerm
                                          .changeOwner(Symbol.spliceOwner)
                                          .asExprOf[FinalizerKeyword]
                                      },
                                  )
                                }.asTerm
                      }
              }
        }
      }
    }

    case class TryFinally(
        expr: KeywordTree,
        finalizer: KeywordTree,
        valueType: TypeRepr
    ) extends KeywordTree {
      lazy val keywordTerm = {
        valueType.usingType {
          [Value] =>
            (tv: quoted.Type[Value]) =>
              given quoted.Type[Value] = tv
              expr.usingKeyword[Term, Value] {
                [TryKeyword, TryValue <: Value] =>
                  (tryKeywordExpr: quoted.Expr[TryKeyword]) =>
                    (
                        tryKeywordType: quoted.Type[TryKeyword],
                        tryValueType: quoted.Type[TryValue]
                    ) =>
                      given quoted.Type[TryKeyword] = tryKeywordType
                      given quoted.Type[TryValue] = tryValueType
                      finalizer.usingKeyword {
                        [FinalizerKeyword, FinalizerValue] =>
                          (finalizerKeywordExpr: quoted.Expr[
                            FinalizerKeyword
                          ]) =>
                            (
                                finalizerKeywordType: quoted.Type[
                                  FinalizerKeyword
                                ],
                                finalizerValueType: quoted.Type[FinalizerValue]
                            ) =>
                              given quoted.Type[FinalizerKeyword] =
                                finalizerKeywordType
                              given quoted.Type[FinalizerValue] =
                                finalizerValueType
                              '{
                                com.thoughtworks.dsl.keywords.TryFinally(
                                  () =>
                                    ${
                                      tryKeywordExpr.asTerm
                                        .changeOwner(Symbol.spliceOwner)
                                        .asExprOf[TryKeyword]
                                    },
                                  () =>
                                    ${
                                      finalizerKeywordExpr.asTerm
                                        .changeOwner(Symbol.spliceOwner)
                                        .asExprOf[FinalizerKeyword]
                                    },
                                )
                              }.asTerm
                    }
            }
        }
      }
    }

    case class TryCatch(
        expr: KeywordTree,
        cases: Seq[(CaseDef, KeywordTree)],
        valueType: TypeRepr
    ) extends KeywordTree {
      lazy val keywordTerm = {
        usingCases[Term, Throwable](cases, valueType) {
          [Set, Value] =>
            (pf: quoted.Expr[PartialFunction[Throwable, Set]]) =>
              (setType: quoted.Type[Set], vt: quoted.Type[Value]) =>
                given quoted.Type[Set] = setType
                given quoted.Type[Value] = vt
                expr.usingKeyword[Term, Value] {
                  [TryKeyword, TryValue <: Value] =>
                    (tryKeywordExpr: quoted.Expr[TryKeyword]) =>
                      (
                          tryKeywordTpe: quoted.Type[TryKeyword],
                          tryValueTpe: quoted.Type[TryValue]
                      ) =>
                        given quoted.Type[TryKeyword] = tryKeywordTpe
                        given quoted.Type[TryValue] = tryValueTpe
                        '{
                          com.thoughtworks.dsl.keywords
                            .TryCatch[TryKeyword, Set](
                              () =>
                                ${
                                  tryKeywordExpr.asTerm
                                    .changeOwner(Symbol.spliceOwner)
                                    .asExprOf[TryKeyword]
                                },
                              $pf
                            )
                        }.asTerm
              }
        }
      }
    }

    case class Suspend(body: KeywordTree) extends KeywordTree {
      export body.valueType
      lazy val keywordTerm = body.usingKeyword {
        [BodyKeyword, BodyValue] =>
          (bodyExpr: quoted.Expr[BodyKeyword]) =>
            (
                bodyKeywordTpe: quoted.Type[BodyKeyword],
                bodyValueTpe: quoted.Type[BodyValue]
            ) =>
              given quoted.Type[BodyKeyword] = bodyKeywordTpe
              given quoted.Type[BodyValue] = bodyValueTpe
              '{
                com.thoughtworks.dsl.keywords.Suspend(() => $bodyExpr)
              }.asTerm
      }
    }

    case class While(cond: KeywordTree, body: KeywordTree) extends KeywordTree {
      def valueType = TypeRepr.of[Unit]
      lazy val keywordTerm = {
        cond.usingKeyword {
          [CondKeyword, CondValue] =>
            (condExpr: quoted.Expr[CondKeyword]) =>
              (
                  condKeywordTpe: quoted.Type[CondKeyword],
                  condValueTpe: quoted.Type[CondValue]
              ) =>
                given quoted.Type[CondKeyword] = condKeywordTpe
                given quoted.Type[CondValue] = condValueTpe
                body.usingKeyword {
                  [BodyKeyword, BodyValue] =>
                    (bodyExpr: quoted.Expr[BodyKeyword]) =>
                      (
                          bodyKeywordTpe: quoted.Type[BodyKeyword],
                          bodyValueTpe: quoted.Type[BodyValue]
                      ) =>
                        given quoted.Type[BodyKeyword] = bodyKeywordTpe
                        given quoted.Type[BodyValue] = bodyValueTpe
                        '{
                          com.thoughtworks.dsl.keywords.While(
                            () =>
                              ${
                                condExpr.asTerm
                                  .changeOwner(Symbol.spliceOwner)
                                  .asExprOf[CondKeyword]
                              },
                            () =>
                              ${
                                bodyExpr.asTerm
                                  .changeOwner(Symbol.spliceOwner)
                                  .asExprOf[BodyKeyword]
                              }
                          )
                        }.asTerm
              }
        }
      }
    }

    case class Return(returnValue: Term, returnValueType: TypeRepr)
        extends KeywordTree {
      def valueType = TypeRepr.of[Nothing]
      lazy val keywordTerm = {
        returnValueType.usingType {
          [A0] =>
            (tpe0: quoted.Type[A0]) =>
              given quoted.Type[A0] = tpe0
              returnValue.usingExpr[Term, A0] {
                [A <: A0] =>
                  (expr: quoted.Expr[A]) =>
                    (tpe: quoted.Type[A]) ?=>
                      '{
                        com.thoughtworks.dsl.keywords.Return[A0]($expr)
                      }.asTerm
            }
        }
      }
    }

    case class Block(body: KeywordTree) extends KeywordTree {
      def valueType = body.valueType
      lazy val keywordTerm = {
        body.keywordTerm
      }
    }

    case class Let(
        blockTemplate: qctx.reflect.Block,
        stat: Statement,
        rest: KeywordTree
    ) extends KeywordTree {
      export rest.valueType
      lazy val keywordTerm = {
        @annotation.tailrec
        def loop(stats: Queue[Statement], rest: KeywordTree): Term = {
          rest match {
            case Let(`blockTemplate`, head, tail) =>
              loop(stats :+ head, tail)
            case otherKeyword =>
              qctx.reflect.Block
                .copy(blockTemplate)(stats.toList, otherKeyword.keywordTerm)
          }
        }
        loop(Queue(stat), rest)
      }
      override def flatMap(flatMapper: Term => KeywordTree): KeywordTree = {
        copy(rest = rest.flatMap(flatMapper))
      }
    }

    case class FlatMap(upstream: KeywordTree, flatMapper: Term => KeywordTree)
        extends KeywordTree {
      lazy val (keywordTerm, valueType) = {
        upstream.usingKeyword {
          [K, V] =>
            (keywordExpr: quoted.Expr[K]) =>
              (keywordTpe: quoted.Type[K], valueTpe: quoted.Type[V]) => {
                given quoted.Type[K] = keywordTpe
                given quoted.Type[V] = valueTpe
                def go[X] = {
                  var innerKeywordTreeOption: Option[KeywordTree] = None
                  val anyFunction = '{ (dslValue: V) =>
                    ${
                      val innerKeywordTree = flatMapper('dslValue.asTerm)
                      innerKeywordTreeOption = Some(innerKeywordTree)
                      innerKeywordTree.keywordTerm
                        .changeOwner(Symbol.spliceOwner)
                        .asExpr
                    }
                  }
                  val Some(innerKeywordTree) = innerKeywordTreeOption
                  given quoted.Type[X] = innerKeywordTree.keywordTerm.tpe.asType
                    .asInstanceOf[quoted.Type[X]]
                  val flatMapperExpr = '{ $anyFunction.asInstanceOf[V => X] }
                  val flatMapperTerm = flatMapperExpr.asTerm
                  val flatMapObject = '{ keywords.FlatMap }.asTerm
                  Apply(
                    TypeApply(
                      Select.unique(flatMapObject, "apply"),
                      List(
                        TypeTree.of[K],
                        TypeTree.of[X]
                      )
                    ),
                    List(keywordExpr.asTerm, flatMapperTerm)
                  ) -> innerKeywordTree.valueType
                }
                go
            }
        }
      }

      override def flatMap(nextFlatMapper: Term => KeywordTree): KeywordTree = {
        upstream.flatMap { term =>
          flatMapper(term).flatMap(nextFlatMapper)
        }
      }
    }

    case class Keyword(keywordTerm: Term, valueType: TypeRepr)
        extends KeywordTree

    case class Pure(term: Term, valueType: TypeRepr) extends KeywordTree {
      override def flatMap(flatMapper: Term => KeywordTree) = flatMapper(term)

      // override def where(blockTemplate: qctx.reflect.Block, statement: Statement) = {
      //   Pure(qctx.reflect.Block.copy(blockTemplate)(List(statement), term))
      // }

      override def block = this

      def keywordTerm: Term = {
        // We don't need widenTermRefByName because Pure is covariant, so it could be widen automatically as needed
        valueType /*.widenTermRefByName*/.usingType {
          [A] =>
            (ta: quoted.Type[A]) =>
              given quoted.Type[A] = ta
              '{
                keywords.Pure[A](${ term.asExprOf[A] })
              }.asTerm
        }
      }
    }

  }

  object Macros {
    def reify[
        ShouldResetNestedFunctions <: Boolean & Singleton,
        DontSuspend <: Boolean & Singleton,
        V
    ](
        body: quoted.Expr[_]
    )(using
        qctx: Quotes,
        translateNestedFunctions: quoted.Type[ShouldResetNestedFunctions],
        dontSuspend: quoted.Type[DontSuspend],
        tv: quoted.Type[V]
    ): quoted.Expr[_] = {
      import quoted.quotes.reflect.*
      quoted.Type.valueOfConstant[ShouldResetNestedFunctions] match {
        case None =>
          report.error("ShouldResetNestedFunctions is not defined", body)
          '{ ??? }
        case Some(translateNestedFunction) =>
          quoted.Type.valueOfConstant[DontSuspend] match {
            case None =>
              report.error("DontSuspend is not defined", body)
              '{ ??? }
            case Some(dontSuspend) =>
              Macros[qctx.type](
                shouldResetNestedFunctions = translateNestedFunction,
                dontSuspend = dontSuspend
              ).reify[V](body /*.underlyingArgument*/ )
          }
      }
    }

    def reset[
        ShouldResetNestedFunctions <: Boolean & Singleton,
        DontSuspend <: Boolean & Singleton,
        From,
        To
    ](
        body: quoted.Expr[From]
    )(using
        qctx: Quotes,
        translateNestedFunctions: quoted.Type[ShouldResetNestedFunctions],
        dontSuspend: quoted.Type[DontSuspend],
        fromType: quoted.Type[From],
        toType: quoted.Type[To]
    ): quoted.Expr[To] = {
      import quoted.quotes.reflect.{_, given}
      quoted.Type.valueOfConstant[ShouldResetNestedFunctions] match {
        case None =>
          report.error("ShouldResetNestedFunctions is not defined", body)
          '{ ??? }
        case Some(translateNestedFunction) =>
          quoted.Type.valueOfConstant[DontSuspend] match {
            case None =>
              report.error("DontSuspend is not defined", body)
              '{ ??? }
            case Some(dontSuspend) =>
              val result = Macros[qctx.type](
                shouldResetNestedFunctions = translateNestedFunction,
                dontSuspend = dontSuspend
              ).reset[From, To](body /*.underlyingArgument*/ )
              // report.warning(result.asTerm.show(using qctx.reflect.Printer.TreeStructure))
              // report.warning(result.asTerm.show)
              result
          }
      }
    }
  }
}
