package com.thoughtworks.dsl
package compilerplugins

import com.thoughtworks.dsl.Dsl.{ResetAnnotation, shift}
import com.thoughtworks.dsl.compilerplugins.BangNotation.HasReturn

import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.typechecker.ContextMode
import scala.tools.nsc.{Global, Mode}
private object BangNotation {
  sealed trait HasReturn
  object HasReturn {
    case object Yes extends HasReturn
    case object No extends HasReturn
  }

}

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

  private val hasScalaJsPlugin =
    global.settings.plugin.value.exists(_.matches("""^.*scalajs-compiler_.*\.jar$"""))

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

  private trait Deactable extends AnalyzerPlugin {
    override def isActive(): Boolean = {
      active && phase.id <= currentRun.typerPhase.id
    }
  }

  /** An [[AnalyzerPlugin]] that replaces trees annatated as [[ResetAnnotation]] to its cps transformed trees */
  private trait TreeResetter extends AnalyzerPlugin with AnnotationSymbols {
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
                  Apply(
                    Select(prefix, termNames.unapply | termNames.unapplySeq),
                    List(Ident(termNames.SELECTOR_DUMMY))
                  ),
                  args
                ) =>
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
      tree.tpe.annotations.collectFirst {
        case annotation if annotation.matches(resetAnnotationSymbol) =>
          val Some(attachment) = tree.attachments.get[CpsAttachment]
          val cpsTree = scalaBug8825Workaround(resetAttrs(attachment(identity)))
//          reporter.info(tree.pos, s"Translating to continuation-passing style: $cpsTree", true)
          deactAnalyzerPlugins {
            typer.context.withMode(ContextMode.ReTyping) {
              typer.typed(Block(Nil, cpsTree), Mode.EXPRmode)
            }
          }
      } match {
        case Some(typedCpsTree) =>
//          reporter.info(tree.pos, s"Translating to continuation-passing style: $typedCpsTree", true)
          typedCpsTree.modifyType(_.filterAnnotations(!_.matches(resetAnnotationSymbol)))
          typedCpsTree.removeAttachment[CpsAttachment]
          typedCpsTree
        case None =>
          tree
      }

    }

  }

  private trait BangNotationTransformer extends AnalyzerPlugin with AnnotationSymbols {

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
      tree match {
        case defDef: DefDef if defDef.name.toString == "randomInt" =>
          reporter.info(tree.pos, show(tree), true)
        case _ =>
      }
      def hasCpsAttachment(child: Any): Boolean = {
        child match {
          case list: List[_]                => list.exists(hasCpsAttachment)
          case TypeApply(fun, args)         => hasCpsAttachment(fun)
          case Apply(fun, args)             => hasCpsAttachment(fun) || args.exists(hasCpsAttachment)
          case CaseDef(pat, guard, body)    => hasCpsAttachment(body)
          case ValDef(mods, name, tpt, rhs) => hasCpsAttachment(rhs)
          case childTree: Tree              => childTree.hasAttachment[CpsAttachment]
          case _                            => false
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

    private def hasReturnTree(tree: Tree): Boolean = {
      tree.attachments.get[HasReturn] match {
        case Some(HasReturn.Yes) =>
          true
        case Some(HasReturn.No) =>
          false

        case _ =>
          tree match {
            case _: Return =>
              true
            case valDef: ValDef =>
              hasReturnTree(valDef.rhs)
            case _: MemberDef =>
              false
            case _ =>
              tree.children.exists(hasReturnTree)
          }
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
          case q"new $constructor[..$typeParameters](...$parameterLists)" =>
            cpsParameterList(parameterLists) { parameterListsValues =>
              continue(atPos(tree.pos) {
                q"new $constructor[..$typeParameters](...$parameterListsValues)"
              })
            }
          case Typed(expr, tpt) =>
            cpsAttachment(expr) { exprValue =>
              atPos(tree.pos) {
                if (tpt.tpe.hasAnnotation(resetAnnotationSymbol)) {
                  continue(exprValue)
                } else {
                  tpt match {
                    case tpt: TypeTree =>
                      // Create a new TypeTree with a null original, as a workaround of https://github.com/ThoughtWorksInc/Dsl.scala/issues/114
                      val noOriginal = TypeTree(tpt.tpe).setPos(tpt.pos)
                      continue(Typed(exprValue, noOriginal))
                    case _ =>
                      continue(Typed(exprValue, tpt))
                  }
                }
              }
            }
          case Block(stats, expr) =>
            def loop(stats: List[Tree]): Tree = {
              stats match {
                case Nil =>
                  cpsAttachment(expr) {
                    case block: Block =>
                      continue(block)
                    case notBlock =>
                      continue(treeCopy.Block(tree, Nil, notBlock))
                  }
                case (valDef @ ValDef(mods, name, tpt, rhs)) :: tail =>
                  // TODO: lazy val
                  cpsAttachment(rhs) { rhsValue =>
                    atPos(valDef.pos) {
                      q"${treeCopy.ValDef(valDef, mods, name, tpt, rhsValue)}; ${loop(tail)}"
                    }
                  }
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
            ${
              val endIfBody = toFunction1(continue, tpe)
              if (hasReturnTree(endIfBody)) {
                q"""
                  val $endIfName = $endIfBody
                """.updateAttachment(HasReturn.Yes)
              } else {
                q"""
                  @${definitions.ScalaInlineClass} def $endIfName = $endIfBody
                """.updateAttachment(HasReturn.No)
              }
            }
            
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
            ${
              val endMatchBody = toFunction1(continue, tpe)
              if (hasReturnTree(endMatchBody)) {
                q"""
                  val $endMatchName = $endMatchBody
                """.updateAttachment(HasReturn.Yes)
              } else {
                q"""
                  @${definitions.ScalaInlineClass} def $endMatchName = $endMatchBody
                """.updateAttachment(HasReturn.No)
              }
            }
            
            ${cpsAttachment(selector) { selectorValue =>
              atPos(tree.pos) {
                Match(
                  selectorValue,
                  cases.map { case caseDef @ CaseDef(pat, guard, body) =>
                    treeCopy.CaseDef(
                      caseDef,
                      pat,
                      guard,
                      cpsAttachment(body) { bodyValue =>
                        q"$endMatchName.apply($bodyValue)"
                      }
                    )
                  }
                )
              }
            }}
            """
          case _: CaseDef =>
            // This CaseDef tree contains some bang notations, and will be translated by enclosing Try or Match tree, not here
            EmptyTree
          case Try(block, catches, finalizer) =>
            val resultName = currentUnit.freshTermName("result")
            val handlerName = currentUnit.freshTermName("handler")

            def transformBlock() =
              q"""{ $handlerName: ${TypeTree()} => ${cpsAttachment(block) { blockValue =>
                q"$handlerName.apply($blockValue)"
              }}}"""

            def transformCatches() =
              q"""{case ..${catches.map { caseDef =>
                atPos(caseDef.pos) {
                  treeCopy.CaseDef(
                    caseDef,
                    caseDef.pat,
                    caseDef.guard,
                    q"""{ $handlerName: ${TypeTree()} => ${{
                      cpsAttachment(caseDef.body) { bodyValue =>
                        q"$handlerName.apply($bodyValue)"
                      }
                    }}}"""
                  )
                }
              }}}"""

            def transformFinalizer() =
              q"""{ $handlerName: ${TypeTree()} =>
              ${cpsAttachment(finalizer) { finalizerValue =>
                q"$handlerName($finalizerValue: ${typeOf[Unit]})"
              }}}"""

            def endTry() = q"""{ ($resultName: $tpe) => ${continue(q"$resultName")}}"""

            finalizer match {
              case EmptyTree =>
                q"""_root_.com.thoughtworks.dsl.Dsl.TryCatch.Ops(${endTry()}).apply(
                  ${transformBlock()},
                  ${transformCatches()}
                )"""
              case _ if catches.isEmpty =>
                q"""_root_.com.thoughtworks.dsl.Dsl.TryFinally.Ops(${endTry()}).apply(
                  ${transformBlock()},
                  ${transformFinalizer()}
                )"""
              case _ =>
                q"""_root_.com.thoughtworks.dsl.Dsl.TryCatchFinally.Ops(${endTry()}).apply(
                  ${transformBlock()},
                  ${transformCatches()},
                  ${transformFinalizer()}
                )"""
            }
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
          case tree =>
            reporter.info(tree.pos, "CPS-transformation is skipped for this tree", force = false)
            continue(tree)
        }
      }

      if (mode.inExprMode) {
        val symbol = tree.symbol
        tree match {
          case q"$shiftOps.$shiftMethod" if symbol != null && symbol.hasAnnotation(shiftSymbol) =>
            def attachment: CpsAttachment = { continue: (Tree => Tree) =>
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
          case _ =>
            if (isCpsTree(tree)) {
              tree.updateAttachment[CpsAttachment](cps)
            }
        }
      }

      tpe
    }

  }

  private trait AnnotationSymbols {
    private[BangNotation] lazy val resetAnnotationSymbol = symbolOf[ResetAnnotation]
    private[BangNotation] lazy val shiftSymbol = symbolOf[shift]
  }

  val name: String = "BangNotation"

  override def init(options: List[String], error: String => Unit): Boolean = {
    super.init(options, error) && {
      try {
        global.analyzer.addAnalyzerPlugin(new Deactable with TreeResetter with BangNotationTransformer)
        true
      } catch {
        case e: ScalaReflectionException =>
          error("""The BangNotation compiler plug-in requires the runtime library:
  libraryDependencies += "com.thoughtworks.dsl" %% "dsl" % "latest.release"
""")
          false
      }
    }
  }

  val description: String =
    "A compiler plugin that converts native imperative syntax to monadic expressions or continuation-passing style expressions"

  val components = Nil

}
