package com.thoughtworks.dsl
package compilerplugins

import com.thoughtworks.dsl.Dsl.{ResetAnnotation, nonTypeConstraintReset, shift}

import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.typechecker.ContextMode
import scala.tools.nsc.{Global, Mode, Phase}

/** The Scala compiler plug-in to convert ordinary Scala control flows to continuation-passing style,
  * which will then be interpreted by [[Dsl]].
  *
  * = Usage =
  *
  * `<pre>
  * // In your build.sbt
  * addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-bangnotation" % "latest.release")
  * </pre>`
  *
  * @author 杨博 (Yang Bo)
  */
final class BangNotation(override val global: Global) extends Plugin {
  import global._
  import global.analyzer._

  private var active = true
  private def deactAnalyzerPlugins[A](run: => A): A = {
    synchronized {
      active = false
      try {
        run
      } finally {
        active = true
      }
    }
  }

  private type CpsAttachment = (Tree => Tree) => Tree

  private val nonTypeConstraintResetSymbol = symbolOf[nonTypeConstraintReset]
  private val resetAnnotationSymbol = symbolOf[ResetAnnotation]
  private val shiftSymbol = symbolOf[shift]

  private trait Deactable extends AnalyzerPlugin {
    override def isActive(): Boolean = {
      active && phase.id < currentRun.picklerPhase.id
    }
  }

  /** An [[AnalyzerPlugin]] that replaces trees annatated as [[ResetAnnotation]] to its cps transformed trees */
  private trait TreeResetter extends AnalyzerPlugin {
    override def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = {
      super.canAdaptAnnotations(tree, typer, mode, pt) || {
        mode.inExprMode && tree.tpe.hasAnnotation(resetAnnotationSymbol) && tree.hasAttachment[CpsAttachment]
      }
    }

    /** Avoid [[UnApply]] in `tree` to suppress compiler crash due to `unexpected UnApply xxx`.
      *
      * @see https://github.com/scala/bug/issues/8825
      */
    private def scalaBug8825Workaround(tree: Tree): Tree = {
      val transformer = new Transformer {
        override def transform(tree: global.Tree): global.Tree = {
          tree match {
            case UnApply(
                Apply(Select(prefix, termNames.unapply | termNames.unapplySeq), List(Ident(termNames.SELECTOR_DUMMY))),
                args) =>
              pq"$prefix(..${transformTrees(args)})"
            case _ =>
              super.transform(tree)
          }
        }
      }
      transformer.transform(tree)
    }

    override def adaptAnnotations(tree0: Tree, typer: Typer, mode: Mode, pt: Type): Tree = {
      val tree = super.adaptAnnotations(tree0, typer, mode, pt)
      val Seq(typedCpsTree) = tree.tpe.annotations.collect {
        case annotation if annotation.matches(resetAnnotationSymbol) =>
          val Some(attachment) = tree.attachments.get[CpsAttachment]
          val cpsTree = scalaBug8825Workaround(resetAttrs(attachment(identity)))
//          reporter.info(tree.pos, s"Translating to continuation-passing style: $cpsTree", true)
          deactAnalyzerPlugins {
            typer.context.withMode(ContextMode.ReTyping) {
              typer.typed(cpsTree, Mode.EXPRmode)
            }
          }
      }
      typedCpsTree.modifyType(_.filterAnnotations(!_.matches(resetAnnotationSymbol)))
    }

  }

  private trait BangNotationTransformer extends AnalyzerPlugin {

    private def cpsAttachment(tree: Tree)(continue: Tree => Tree): Tree = {
      tree.attachments.get[CpsAttachment] match {
        case Some(attachment) => attachment(continue)
        case None             => continue(tree)
      }
    }
    private def cpsParameter(parameters: List[Tree])(continue: List[Tree] => Tree): Tree = {
      parameters match {
        case Nil =>
          continue(Nil)
        case head :: tail =>
          cpsAttachment(head) { headValue =>
            cpsParameter(tail) { tailValues =>
              continue(headValue :: tailValues)
            }
          }
      }
    }
    private def cpsParameterList(parameterLists: List[List[Tree]])(continue: List[List[Tree]] => Tree): Tree = {
      parameterLists match {
        case Nil =>
          continue(Nil)
        case headList :: tailList =>
          cpsParameter(headList) { headValue =>
            cpsParameterList(tailList) { tailValues =>
              continue(headValue :: tailValues)
            }
          }
      }
    }

    private def isCpsTree(tree: Tree) = {
      def hasCpsAttachment(child: Any): Boolean = {
        child match {
          case list: List[_]             => list.exists(hasCpsAttachment)
          case CaseDef(pat, guard, body) => hasCpsAttachment(body)
          case childTree: Tree           => childTree.hasAttachment[CpsAttachment]
          case _                         => false
        }
      }
      tree.productIterator.exists(hasCpsAttachment)
    }

    private val whileName = currentUnit.freshTermName("while")
    private val whileDef = {
      val domainName = currentUnit.freshTypeName("Domain")
      val conditionName = currentUnit.freshTermName("condition")
      val conditionValueName = currentUnit.freshTermName("conditionValue")
      val bodyName = currentUnit.freshTermName("body")
      val endWhileName = currentUnit.freshTermName("endWhile")
      q"""
      @${definitions.ScalaInlineClass} def $whileName[$domainName]($endWhileName: => $domainName)(
        $bodyName: (=> $domainName) => $domainName,
        $conditionName: (_root_.scala.Boolean => $domainName) => $domainName): $domainName = {
        $conditionName { $conditionValueName: ${TypeTree()} =>
          if ($conditionValueName) {
            $bodyName {
              $whileName[$domainName]($endWhileName)($bodyName, $conditionName)
            }
          } else {
            $endWhileName
          }
        }
      }
      """
    }
    private val doWhileName = currentUnit.freshTermName("doWhile")

    private val doWhileDef = {
      val domainName = currentUnit.freshTypeName("Domain")
      val conditionName = currentUnit.freshTermName("condition")
      val conditionValueName = currentUnit.freshTermName("conditionValue")
      val bodyName = currentUnit.freshTermName("body")
      val endDoWhileName = currentUnit.freshTermName("endDoWhile")
      q"""
      @${definitions.ScalaInlineClass} def $doWhileName[$domainName]($endDoWhileName: => $domainName)(
        $bodyName: (=> $domainName) => $domainName,
        $conditionName: (_root_.scala.Boolean => $domainName) => $domainName): $domainName = {
        $bodyName {
          $conditionName { $conditionValueName: ${TypeTree()} =>
            if ($conditionValueName) {
              $doWhileName[$domainName]($endDoWhileName)($bodyName, $conditionName)
            } else {
              $endDoWhileName
            }
          }
        }
      }
      """
    }
    private def notPure(head: Tree): List[Tree] = {
      head match {
        case (_: Ident) | (_: Literal) =>
          Nil
        case _ =>
          head :: Nil
      }
    }

    private def toFunction1(continue: Tree => Tree, parameterTypes: Type) = {
      val parameterName = currentUnit.freshTermName("a")
      val id = q"$parameterName"
      continue(id) match {
        case q"${f: Ident}.apply(${`id`})" =>
          f
        case transformed =>
          q"""
          { ($parameterName: $parameterTypes) =>
            ${transformed}
          }
          """
      }
    }
    override def pluginsTyped(tpe0: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      val tpe = super.pluginsTyped(tpe0, typer, tree, mode, pt)

      def cps(continue: Tree => Tree): Tree = atPos(tree.pos) {
        tree match {
          case q"$prefix.$methodName[..$typeParameters](...$parameterLists)" =>
            cpsAttachment(prefix) { prefixValue =>
              cpsParameterList(parameterLists) { parameterListsValues =>
                continue(atPos(tree.pos) {
                  q"$prefixValue.$methodName[..$typeParameters](...$parameterListsValues)"
                })
              }
            }

          case q"${method: Ident}[..$typeParameters](...$parameterLists)" =>
            cpsParameterList(parameterLists) { parameterListsValues =>
              continue(atPos(tree.pos) {
                q"$method[..$typeParameters](...$parameterListsValues)"
              })
            }
          // TODO: lazy val
          case ValDef(mods, name, tpt, rhs) =>
            cpsAttachment(rhs) { rhsValue =>
              atPos(tree.pos) {
                q"""
                ${treeCopy.ValDef(tree, mods, name, tpt, rhsValue)}
                ${continue(q"()")}
                """
              }
            }
          case Typed(expr, tpt) =>
            cpsAttachment(expr) { exprValue =>
              atPos(tree.pos) {
                if (tpt.tpe.hasAnnotation(resetAnnotationSymbol)) {
                  continue(exprValue)
                } else {
                  continue(Typed(exprValue, tpt))
                }
              }
            }
          case Block(stats, expr) =>
            def loop(stats: List[Tree]): Tree = {
              stats match {
                case Nil =>
                  cpsAttachment(expr)(continue)
                case head :: tail =>
                  cpsAttachment(head) { headValue =>
                    q"..${notPure(headValue)}; ${loop(tail)}"
                  }
              }
            }

            loop(stats)
          case If(cond, thenp, elsep) =>
            val endIfName = currentUnit.freshTermName("endIf")
            q"""
            @${definitions.ScalaInlineClass} val $endIfName = ${toFunction1(continue, tpe)}
            ${cpsAttachment(cond) { condValue =>
              atPos(tree.pos) {
                q"""
                if ($condValue) ${cpsAttachment(thenp) { result =>
                  q"$endIfName.apply($result)"
                }} else ${cpsAttachment(elsep) { result =>
                  q"$endIfName.apply($result)"
                }}
                """
              }
            }}
            """
          case Match(selector, cases) =>
            val endMatchName = currentUnit.freshTermName("endMatch")
            q"""
            @${definitions.ScalaInlineClass} val $endMatchName = ${toFunction1(continue, tpe)}
            ${cpsAttachment(selector) { selectorValue =>
              atPos(tree.pos) {
                Match(
                  selectorValue,
                  cases.map {
                    case caseDef @ CaseDef(pat, guard, body) =>
                      treeCopy.CaseDef(caseDef, pat, guard, cpsAttachment(body) { bodyValue =>
                        q"$endMatchName.apply($bodyValue)"
                      })
                  }
                )
              }
            }}
            """
          case _: CaseDef =>
            // This CaseDef tree contains some bang notations, and will be translated by enclosing Try or Match tree, not here
            EmptyTree
          case Try(block, catches, finalizer) =>
            val finalizerName = currentUnit.freshTermName("finalizer")
            val tryResultName = currentUnit.freshTermName("tryResult")
            val scopeApplyName = currentUnit.freshTermName("scopeApply")
            val unhandledExceptionName = currentUnit.freshTermName("unhandledException")

            q"""
            @${definitions.ScalaInlineClass} def $scopeApplyName[Domain, Value](continue: Value => Domain)(
            continuation: (Value => Domain) => Domain)(
            implicit scopeDsl: _root_.com.thoughtworks.dsl.Dsl[_root_.com.thoughtworks.dsl.keywords.Scope[Domain, Value],Domain,Value]
            )= {
              _root_.com.thoughtworks.dsl.keywords.Scope(continuation).cpsApply(continue)
            }

            $scopeApplyName { ($tryResultName: $tpe) => ${{
              cpsAttachment(finalizer) { finalizerValue =>
                q"""
                  ..${notPure(finalizerValue)}
                  ${continue(q"$tryResultName")}
                """
              }
            }}} { ($finalizerName: ${TypeTree()}) =>
              _root_.com.thoughtworks.dsl.keywords.Catch {
                case ..${{
              catches.map { caseDef =>
                atPos(caseDef.pos) {
                  treeCopy.CaseDef(
                    caseDef,
                    caseDef.pat,
                    caseDef.guard,
                    cpsAttachment(caseDef.body) { bodyValue =>
                      q"$finalizerName.apply($bodyValue)"
                    }
                  )
                }
              }
            }}
              }.cpsApply { _: _root_.scala.Unit => ${{
              cpsAttachment(block) { blockValue =>
                q"""
                  ${q"$finalizerName.apply($blockValue)"}
                  """
              }
            }}}
            }
            """
          case Assign(lhs, rhs) =>
            cpsAttachment(rhs) { rhsValue =>
              continue(treeCopy.Assign(tree, lhs, rhsValue))
            }
          case q"while($condition) $body" =>
            // TODO: Trampoline
            val continueName = currentUnit.freshTermName("continue")
            val conditionHandlerName = currentUnit.freshTermName("conditionHandler")
            q"""
            $whileDef
            $whileName({
              ${continue(q"()")}
            })({ $continueName: ${TypeTree()} => ${cpsAttachment(body) { bodyValue =>
              q"""
                ..${notPure(bodyValue)}
                $continueName
              """
            }}},
            { $conditionHandlerName: ${TypeTree()} => ${cpsAttachment(condition) { conditionValue =>
              q"$conditionHandlerName.apply($conditionValue)"
            }}})
            """
          case q"do $body while($condition)" =>
            // TODO: Trampoline
            val continueName = currentUnit.freshTermName("continue")
            val conditionHandlerName = currentUnit.freshTermName("conditionHandler")
            q"""
            $doWhileDef

            $doWhileName({
              ${continue(q"()")}
            })({ $continueName: ${TypeTree()} => ${cpsAttachment(body) { bodyValue =>
              q"""
                ..${notPure(bodyValue)}
                $continueName
              """
            }}},
            { $conditionHandlerName: ${TypeTree()} => ${cpsAttachment(condition) { conditionValue =>
              q"$conditionHandlerName.apply($conditionValue)"
            }}})
            """
          case Throw(expr) =>
            cpsAttachment(expr) { exprValue =>
              continue(treeCopy.Throw(tree, exprValue))
            }
          case Return(expr) =>
            cpsAttachment(expr) { exprValue =>
              continue(treeCopy.Return(tree, exprValue))
            }
        }
      }

      if (mode.inExprMode) {
        val symbol = tree.symbol
        if (symbol != null && symbol.hasAnnotation(shiftSymbol) && !tree.isDef) {
          val q"$shiftOps.$shiftMethod" = tree
          def attachment: CpsAttachment = { continue: (Tree => Tree) =>
            val aName = currentUnit.freshTermName("a")

            // FIXME: tpe is a by-name type. I don't know why.
            cpsAttachment(shiftOps) { shiftOpsValue =>
              atPos(tree.pos) {
                q"""
                $shiftOpsValue.cpsApply(${toFunction1(continue, tpe)})
              """
              }
            }
          }
          tree.updateAttachment[CpsAttachment](attachment)
        } else if (isCpsTree(tree)) {
          tree.updateAttachment[CpsAttachment](cps)
        }
      }

      tpe
    }

  }

  val name: String = "BangNotation"

  override def init(options: List[String], error: String => Unit): Boolean = {
    global.analyzer.addAnalyzerPlugin(new Deactable with TreeResetter with BangNotationTransformer)
    true
  }

  val description: String =
    "A compiler plugin that converts native imperative syntax to monadic expressions or continuation-passing style expressions"

  val components = Nil

}
