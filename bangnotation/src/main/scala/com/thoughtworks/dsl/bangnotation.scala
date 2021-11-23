package com.thoughtworks
package dsl
import keywords._
import com.thoughtworks.dsl.keywords._, Match._
import Dsl.IsKeyword
import Dsl.Typed.given
import scala.quoted._
import collection.immutable.Queue
import Dsl.given
import scala.util.control.Exception.Catcher
object bangnotation {

  private class Macros[Q <: QuoteContext](resetDescendant: Boolean)(given val qctx: Q) {
    import qctx.tasty.{_, given}

    def reify(body: Expr[_]): Expr[_] = {
      val bodyTerm = body.unseal
      val reifiedTerm = KeywordTree(bodyTerm).keywordTerm
      bodyTerm.tpe.usingType { [V] => (given _: quoted.Type[V]) =>
        reifiedTerm.usingExpr { [K] => (k: Expr[K]) => (given _: quoted.Type[K]) =>
          '{Dsl.Typed.cast[K, V]($k)}: Expr[_]
        }
      }
    }

    def resetDefDef(defDef: DefDef): DefDef = {
      val DefDef(name, typeParams, paramss, tpt, rhsOption) = defDef
      rhsOption match {
        case Some(rhs) if resetDescendant =>
          rhs match {
            case matchTree @ qctx.tasty.Match(scrutinee, cases) =>
              DefDef.copy(defDef)(
                name, typeParams, paramss, tpt, Some(
                  qctx.tasty.Match.copy(matchTree)(
                    scrutinee,
                    cases.map {
                      case caseDef @ CaseDef(pattern, guard, caseRhs) =>
                        CaseDef.copy(caseDef)(pattern, guard, resetTerm(caseRhs))
                    }
                  )
                )
              )
            case _ =>
              DefDef.copy(defDef)(
                name, typeParams, paramss, tpt, Some(resetTerm(rhs))
              )
          }
        case _ =>
          defDef
      }
    }

    def resetTerm(term: Term): Term = {
      term.usingExpr { [Value] => (body: Expr[Value]) => (given _: quoted.Type[Value]) =>
        reset[Value, Value](body).unseal
      }
    }

    def reset[Value, Domain](body: Expr[Value])(given valueType: quoted.Type[Value], domainType: quoted.Type[Domain]): Expr[Domain] = {
      KeywordTree(body.unseal) match {
        case Pure(pure, _) if valueType.unseal.tpe <:< domainType.unseal.tpe =>
          pure.seal.cast[Domain]
        case keywordTree =>
          val reifiedTerm = keywordTree.keywordTerm
          reifiedTerm.usingExpr { [K] => (k: Expr[K]) => (given keywordType: quoted.Type[K]) =>
            {
              searchImplicit('[Dsl.Run[K, Domain, Value]].unseal.tpe) match {
                case success: ImplicitSearchSuccess =>
                  '{
                    ${success.tree.seal.cast[Dsl.Run[K, Domain, Value]]}.apply($k)
                  }
                case failure: ImplicitSearchFailure =>
                  error(s"The keyword ${keywordType.show} is not supported in a `reset` block that returns ${summon[quoted.Type[Domain]].show}\n${failure.explanation}", rootPosition)
                  body.cast[Domain]
              }
            }
          }
        }
    }

    def[B, Bound](tpe: Type)usingType(f: [A <: Bound] => (given quoted.Type[A]) => B): B = {
      f(given tpe.seal.asInstanceOf)
    }

    def[B, UpperBound](term: Term)usingExpr(f: [A <: UpperBound] => Expr[A] => (given quoted.Type[A]) => B): B = {
      def helper[A <: UpperBound] = {
        implicit val tpe: quoted.Type[A] = term.tpe.seal.asInstanceOf[quoted.Type[A]]
        f[A](term.seal.cast[A])(given tpe)
      }
      helper
    }

    val bangSymbols = Symbol.classSymbol("com.thoughtworks.dsl.bangnotation$").method("unary_!").toSet

    sealed trait KeywordTree {
      def keywordTerm: Term
      def valueType: Type

      def where(blockTemplate: qctx.tasty.Block, statement: Statement): KeywordTree = Let(blockTemplate, statement, this)
      def block: KeywordTree = Block(this)
      def usingKeyword[B, Bound](f: [K, V <: Bound] => Expr[K] => (given quoted.Type[K], quoted.Type[V]) => B): B = {
        keywordTerm.usingExpr[B, Any]{ [K] => (keyword: Expr[K]) => (given keywordType: quoted.Type[K]) =>
          {
            valueType.usingType[B, Bound]{ [V <: Bound] => (given valueType: quoted.Type[V]) =>
              f(keyword)(given keywordType, valueType)
            }
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
          case
            Apply(
              Apply(
                TypeApply(
                  id: Ident,
                  List(_, valueTypeTree)
                ),
                List(
                  keyword
                )
              ),
              List(
                _ // given IsKeyword
              )
            )
          if bangSymbols(id.symbol)  =>
            KeywordTree(keyword).flatMap(Keyword(_, valueTypeTree.tpe))
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
              Pure(SelectOuter.copy(selectOuter)(pureQualifier, tpe.typeSymbol.name, levels), term.tpe)
            }
          case (_: Literal) | (_: Ident) | (_: New) | (_: This) | (_: Super) | (_: Closure) =>
            Pure(term, term.tpe)
          case block @ qctx.tasty.Block(
            List(
              defDef: DefDef
            ),
            closure @ Closure(ident: Ident, _)
          ) if (ident.name == defDef.name) =>
            Pure(qctx.tasty.Block.copy(block)(
              List(resetDefDef(defDef)),
              closure,
            ), term.tpe)
          case block @ qctx.tasty.Block(stats, expr) =>
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
                        loop(tail).where(block, ValDef.copy(headVal)(name, tpt, Some(pureRhs)))
                      }
                    case defDef: DefDef =>
                      loop(tail).where(block, resetDefDef(defDef))
                    case _ =>
                      loop(tail).where(block, head)
                    }
              }
            }
            loop().block
          case qctx.tasty.If(cond, thenp, elsep) =>
            If(
              KeywordTree(cond),
              Suspend(KeywordTree(thenp)),
              Suspend(KeywordTree(elsep)),
              term.tpe
            )
          case qctx.tasty.Match(upstream, cases) =>
            Match(
              KeywordTree(upstream),
              cases.map {
                case caseDef @ CaseDef(pattern, guard, body) =>
                  caseDef -> KeywordTree(body)
              },
              term.tpe
            )
          case Try(expr, cases, None) =>
            TryCatch(
              Suspend(KeywordTree(expr)),
              cases.map {
                case caseDef @ CaseDef(pattern, guard, body) =>
                  caseDef -> KeywordTree(body)
              },
              term.tpe
            )
          case Try(expr, Nil, Some(finalizer)) =>
            TryFinally(
              Suspend(KeywordTree(expr)),
              KeywordTree(finalizer),
              term.tpe
            )
          case Try(expr, cases, Some(finalizer)) =>
            TryCatchFinally(
              Suspend(KeywordTree(expr)),
              cases.map {
                case caseDef @ CaseDef(pattern, guard, body) =>
                  caseDef -> KeywordTree(body)
              },
              KeywordTree(finalizer),
              term.tpe
            )
          case typed @ qctx.tasty.Typed(expr, tpt) =>
            // Typed(KeywordTree(expr), typed)
            KeywordTree(expr).flatMap { pureExpr =>
              Pure(qctx.tasty.Typed.copy(typed)(pureExpr, tpt), term.tpe)
            }
          case assign @ Assign(lhs, rhs) =>
            KeywordTree(lhs).flatMap { pureLhs =>
              KeywordTree(rhs).flatMap { pureRhs =>
                Pure(Assign.copy(assign)(pureLhs, pureRhs), term.tpe)
              }
            }
          case whileTerm @ qctx.tasty.While(cond, body) =>
            While(Suspend(KeywordTree(cond)), Suspend(KeywordTree(body)))
          case returnTree @ qctx.tasty.Return(expr) =>
            KeywordTree(expr).flatMap { pureExpr =>
              Return(pureExpr, expr.tpe)
            }
          // case returnTree @ qctx.tasty.Return(expr) =>
          //   KeywordTree(expr).flatMap { pureExpr =>
          //     Pure(qctx.tasty.Return.copy(returnTree)(pureExpr), term.tpe)
          //   }
        }
      }
    
    }

    private def usingCases[R, A](
      cases: Seq[(CaseDef, KeywordTree)], caseValueType: Type
    )(
      f: [Set, Value] => Expr[PartialFunction[A, Set]] => (given quoted.Type[Set], quoted.Type[Value]) => R
    )(
      given quoted.Type[A]
    ) = {
      // val keywordTerms = cases.map { (caseDef, keywordTree) =>
      //   caseDef -> keywordTree.keywordTerm
      // }
      val caseSetType = cases.view.zipWithIndex.foldRight('[Nothing].unseal.tpe) {
        case (((caseDef, keywordTree), i), accumulator) =>
          val indexType = Expr(i).unseal.tpe
          val AppliedType(withIndex, _) = '[WithIndex[_, _]].unseal.tpe
          val AppliedType(ccons, _) = '[Any +: Nothing].unseal.tpe
          AppliedType(
            ccons,
            List(
              AppliedType(
                withIndex,
                List(indexType, keywordTree.keywordTerm.tpe)
              ),
              accumulator
            )
          )          
      }
      caseValueType.usingType { [Value] => (given valueTpe: quoted.Type[Value]) =>
        caseSetType.usingType { [Set] => (given setTpe: quoted.Type[Set]) =>
          '{
            {
              case e: Throwable if false => ???
            }: PartialFunction[A, Set]
          }.underlyingArgument.unseal match {
            case qctx.tasty.Typed(
              pfBlock @ qctx.tasty.Block(
                List(
                  defDef @ DefDef(name, typeParams, paramss, returnTpt, Some(
                    matchTree @ qctx.tasty.Match(
                      scrutinee,
                      List(caseDefTemplate)
                    )
                  ))
                ),
                closure
              ),
              _
            ) =>
              val newCases = cases.view.zipWithIndex.map {
                case ((caseDef, caseKeywordTree), i) =>
                  caseKeywordTree.usingKeyword {
                    [BodyKeyword, BodyValue] =>
                    (bodyExpr: Expr[BodyKeyword]) =>
                    (given bodyKeywordTpe: quoted.Type[BodyKeyword], bodyValueTpe: quoted.Type[BodyValue]) =>
                    Expr(i).unseal.usingExpr[CaseDef, Int] { [Index <: Int] => (indexExpr: Expr[Index]) => (given indexType: quoted.Type[Index]) =>
                      CaseDef.copy(caseDef)(
                        caseDef.pattern match {
                          case Bind(name, pattern) =>
                            Bind.copy(caseDefTemplate.pattern)(name, pattern)
                          case otherPattern =>
                            otherPattern
                        },
                        caseDef.guard,
                        '{WithIndex[Index, BodyKeyword]($indexExpr, $bodyExpr).asInstanceOf[Set]}.unseal
                      )
                    }
                  }
              }.toList
              val pfTerm = qctx.tasty.Block.copy(pfBlock)(List(DefDef.copy(defDef)(name, typeParams, paramss, returnTpt, Some(qctx.tasty.Match.copy(matchTree)(scrutinee, newCases)))), closure)
              f[Set, Value](pfTerm.seal.cast[PartialFunction[A, Set]])
          }
        }
      }
    }

    case class If(cond: KeywordTree, thenp: KeywordTree, elsep: KeywordTree, valueType: Type) extends KeywordTree {
      lazy val keywordTerm = cond.usingKeyword[Term, Boolean] {
        [CondKeyword, CondValue <: Boolean] =>
        (condExpr: Expr[CondKeyword]) =>
        (given condKeywordType: quoted.Type[CondKeyword], condValueType: quoted.Type[CondValue]) =>
        thenp.usingKeyword[Term, Boolean] {
          [ThenpKeyword, ThenpValue <: Boolean] =>
          (thenpExpr: Expr[ThenpKeyword]) =>
          (given thenpKeywordType: quoted.Type[ThenpKeyword], thenpValueType: quoted.Type[ThenpValue]) =>
          elsep.usingKeyword[Term, Boolean] {
            [ElsepKeyword, ElsepValue <: Boolean] =>
            (elsepExpr: Expr[ElsepKeyword]) =>
            (given elsepKeywordType: quoted.Type[ElsepKeyword], elsepValueType: quoted.Type[ElsepValue]) =>
            valueType.usingType { [Result] => (given resultType: quoted.Type[Result]) =>
              '{
                com.thoughtworks.dsl.keywords.If(
                  $condExpr,
                  $thenpExpr,
                  $elsepExpr,
                )
              }.unseal
            }
          }
        }
      }
    }

    case class Match(expr: KeywordTree, cases: Seq[(CaseDef, KeywordTree)], valueType: Type) extends KeywordTree {
      lazy val keywordTerm = {
        expr.usingKeyword { [K, V] =>
          (exprKeywordExpr: Expr[K]) =>
          (given exprKeywordType: quoted.Type[K], exprValueType: quoted.Type[V]) =>
          usingCases[Term, V](cases, valueType) {
            [CaseSet, CaseValue] => 
            (pf: Expr[PartialFunction[V, CaseSet]]) =>
            (given caseSetType: quoted.Type[CaseSet], caseValueType: quoted.Type[CaseValue]) =>
            '{
              com.thoughtworks.dsl.keywords.Match(
                $exprKeywordExpr,
                $pf
              )
            }.unseal
          }
        }
      }
    }

    case class TryCatchFinally(expr: KeywordTree, cases: Seq[(CaseDef, KeywordTree)], finalizer: KeywordTree, valueType: Type) extends KeywordTree {
      lazy val keywordTerm = {
        usingCases[Term, Throwable](cases, valueType) {
          [Set, Value] => 
          (pf: Expr[PartialFunction[Throwable, Set]]) =>
          (given setType: quoted.Type[Set], valueType: quoted.Type[Value]) =>
          expr.usingKeyword[Term, Value] { [K, V <: Value] =>
            (tryKeywordExpr: Expr[K]) =>
            (given tryKeywordTpe: quoted.Type[K], tryValueTpe: quoted.Type[V]) =>
            finalizer.usingKeyword {
              [FinalizerKeyword, FinalizerValue] =>
              (finalizerKeywordExpr: Expr[FinalizerKeyword]) =>
              (given finalizerKeywordType: quoted.Type[FinalizerKeyword], finalizerValueType: quoted.Type[FinalizerValue]) =>
              '{
                com.thoughtworks.dsl.keywords.TryCatchFinally(
                  $tryKeywordExpr,
                  $pf,
                  $finalizerKeywordExpr
                )
              }.unseal
            }
          }
        }
      }
    }

    case class TryFinally(expr: KeywordTree, finalizer: KeywordTree, valueType: Type) extends KeywordTree {
      lazy val keywordTerm = {
        valueType.usingType { [Value] => (given _: quoted.Type[Value]) =>
          expr.usingKeyword[Term, Value] {
            [K, V <: Value] =>
            (tryKeywordExpr: Expr[K]) =>
            (given tryKeywordType: quoted.Type[K], tryValueType: quoted.Type[V]) =>
            finalizer.usingKeyword {
              [FinalizerKeyword, FinalizerValue] =>
              (finalizerKeywordExpr: Expr[FinalizerKeyword]) =>
              (given finalizerKeywordType: quoted.Type[FinalizerKeyword], finalizerValueType: quoted.Type[FinalizerValue]) =>
              '{
                com.thoughtworks.dsl.keywords.TryFinally(
                  $tryKeywordExpr,
                  $finalizerKeywordExpr
                )
              }.unseal
            }
          }
        }
      }
    }

    case class TryCatch(expr: KeywordTree, cases: Seq[(CaseDef, KeywordTree)], valueType: Type) extends KeywordTree {
      lazy val keywordTerm = {
        usingCases[Term, Throwable](cases, valueType) {
          [Set, Value] => 
          (pf: Expr[PartialFunction[Throwable, Set]]) =>
          (given setType: quoted.Type[Set], _: quoted.Type[Value]) =>
          expr.usingKeyword[Term, Value] { [K, V <: Value] =>
            (tryKeywordExpr: Expr[K]) =>
            (given tryKeywordTpe: quoted.Type[K], tryValueTpe: quoted.Type[V]) =>
            '{
              com.thoughtworks.dsl.keywords.TryCatch[K, Set](
                $tryKeywordExpr,
                $pf
              )
            }.unseal
          }
        }
      }
    }

    case class Suspend(body: KeywordTree) extends KeywordTree {
      export body.valueType
      lazy val keywordTerm = body.usingKeyword {
        [BodyKeyword, BodyValue] =>
        (bodyExpr: Expr[BodyKeyword]) =>
        (given bodyKeywordTpe: quoted.Type[BodyKeyword], bodyValueTpe: quoted.Type[BodyValue]) =>
        '{
          com.thoughtworks.dsl.keywords.Suspend(() => $bodyExpr)
        }.unseal
      }
    }

    case class While(cond: KeywordTree, body: KeywordTree) extends KeywordTree {
      export defn.{UnitType => valueType}
      lazy val keywordTerm = {
        cond.usingKeyword{
          [CondKeyword, CondValue] =>
          (condExpr: Expr[CondKeyword]) =>
          (given condKeywordTpe: quoted.Type[CondKeyword], condValueTpe: quoted.Type[CondValue]) =>
          body.usingKeyword {
            [BodyKeyword, BodyValue] =>
            (bodyExpr: Expr[BodyKeyword]) =>
            (given bodyKeywordTpe: quoted.Type[BodyKeyword], bodyValueTpe: quoted.Type[BodyValue]) =>
            '{
              com.thoughtworks.dsl.keywords.While($condExpr, $bodyExpr)
            }.unseal          
          }
        }
      }
    }

    case class Return(returnValue: Term, returnValueType: Type) extends KeywordTree {
      export defn.{NothingType => valueType}
      lazy val keywordTerm = {
        returnValueType.usingType { [A0] => (given tpe: quoted.Type[A0]) =>
          returnValue.usingExpr[Term, A0] { [A <: A0] => (expr: Expr[A]) => (given tpe: quoted.Type[A]) =>
            '{
              com.thoughtworks.dsl.keywords.Return[A0]($expr)
            }.unseal
          }
        }
      }
    }

    case class Block(body: KeywordTree) extends KeywordTree {
      export body.valueType
      lazy val keywordTerm = {
        body.keywordTerm
      }
    }

    case class Let(blockTemplate: qctx.tasty.Block, stat: Statement, rest: KeywordTree) extends KeywordTree {
      export rest.valueType
      lazy val keywordTerm = {
        @annotation.tailrec
        def loop(stats: Queue[Statement], rest: KeywordTree): Term = {
          rest match {
            case Let(`blockTemplate`, head, tail) =>
              loop(stats :+ head, tail)
            case otherKeyword =>
              qctx.tasty.Block.copy(blockTemplate)(stats.toList, otherKeyword.keywordTerm)
          }
        }
        loop(Queue(stat), rest)
      }
      override def flatMap(flatMapper: Term => KeywordTree): KeywordTree = {
        copy(rest = rest.flatMap(flatMapper))
      }
    }

    case class FlatMap(upstream: KeywordTree, flatMapper: Term => KeywordTree) extends KeywordTree {
      lazy val (keywordTerm, valueType) = {
        upstream.usingKeyword {
          [K, V] =>
          (keywordExpr: Expr[K]) =>
          (given keywordTpe: quoted.Type[K], valueTpe: quoted.Type[V]) =>
          {
            var innerKeywordTreeOption: Option[KeywordTree] = None
            val flatMapperTerm = '{ (x: $valueTpe) =>
              ${
                val innerKeywordTree = flatMapper('x.unseal)
                innerKeywordTreeOption = Some(innerKeywordTree)
                innerKeywordTree.keywordTerm.seal
              }
            }.unseal
            val Some(innerKeywordTree) = innerKeywordTreeOption
            val flatMapObject = '{keywords.FlatMap}.unseal
            Apply(TypeApply(Select.unique(flatMapObject, "apply"), List(
              keywordTpe.unseal,
              valueTpe.unseal,
              Inferred(innerKeywordTree.keywordTerm.tpe),
            )), List(keywordExpr.unseal, flatMapperTerm)) -> innerKeywordTree.valueType
          }
        }
      }

      override def flatMap(nextFlatMapper: Term => KeywordTree): KeywordTree = {
        upstream.flatMap { term =>
          flatMapper(term).flatMap(nextFlatMapper)
        }
      }
    }

    // case class Typed(originalTree: KeywordTree, template: qctx.tasty.Typed) extends KeywordTree {
    //   def valueType = template.tpe
    //   override def flatMap(flatMapper: Term => KeywordTree): KeywordTree = {
    //     originalTree.flatMap { term =>
    //       flatMapper(qctx.tasty.Typed.copy(template)(term, template.tpt))
    //     }
    //   }
    //   lazy val keywordTerm = {
    //     val typedObject = '{Dsl.Typed}.unseal
    //     Apply(TypeApply(Select.unique(typedObject, "apply"), List(Inferred(originalTree.keywordTerm.tpe), Inferred(valueType))), List(originalTree.keywordTerm))
    //   }
    // }

    case class Keyword(keywordTerm: Term, valueType: Type) extends KeywordTree

    case class Pure(term: Term, valueType: Type) extends KeywordTree {
      override def flatMap(flatMapper: Term => KeywordTree) = flatMapper(term)

      // override def where(blockTemplate: qctx.tasty.Block, statement: Statement) = {
      //   Pure(qctx.tasty.Block.copy(blockTemplate)(List(statement), term))
      // }

      override def block = this

      def keywordTerm: Term = {
        term.usingExpr { [A] => (expr: Expr[A]) => (given _: quoted.Type[A]) =>
          '{
            keywords.Pure.cast($expr)
          }.unseal
        }
      }
    }

    
  }

  object Macros {
    def reify(body: Expr[_])(given qctx: QuoteContext): Expr[_] = {
      Macros[qctx.type](resetDescendant = false).reify(body.underlyingArgument)
    }

    def reset[From, To](body: Expr[From])(given qctx: QuoteContext, fromType: Type[From], toType: Type[To]): Expr[To] = {
      import qctx.tasty.{_, given}
      val result: Expr[To] = Macros[qctx.type](resetDescendant = false).reset(body.underlyingArgument)
      // warning(result.unseal.show, rootPosition)
      // warning(result.unseal.showExtractors, rootPosition)
      result
    }

  }

  inline def reify(value: => Any) <: Any = ${
    Macros.reify('value)
  }

  class *[Functor[?]] {
    inline def apply[Value](value: => Value): Functor[Value] = ${
      Macros.reset[Value, Functor[Value]]('value)
    }
  }
  inline def *[Domain[_]]: *[Domain] = new *[Domain]

  inline def reset[Value](value: => Value): Value = ${
    Macros.reset[Value, Value]('value)
  }

  @annotation.compileTimeOnly("""This method must be called only inside a `reset` or `*` code block.""")
  def[Keyword, Value](keyword: Keyword)unary_!(given IsKeyword[Keyword, Value]): Value = ???

}        
