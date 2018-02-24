package com.thoughtworks.dsl

import com.thoughtworks.dsl.annotations.{reset, shift}

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
  * addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugin" % "latest.release")
  * </pre>`
  *
  * @author 杨博 (Yang Bo)
  */
final class CompilerPlugin(override val global: Global) extends Plugin {
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

  private val resetSymbol = symbolOf[reset]
  private val shiftSymbol = symbolOf[shift]

  private val cpsAnalyzerPlugin: AnalyzerPlugin = new AnalyzerPlugin {

    override def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = {
      mode.inExprMode && tree.tpe.hasAnnotation(resetSymbol) && tree.hasAttachment[CpsAttachment]
    }

    override def adaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Tree = {
      val Seq(typedCpsTree) = tree.tpe.annotations.collect {
        case annotation if annotation.matches(resetSymbol) =>
          val Some(attachment) = tree.attachments.get[CpsAttachment]
          val cpsTree = resetAttrs(attachment(identity))
//          reporter.info(tree.pos, s"Translating to continuation-passing style: $cpsTree", true)
          deactAnalyzerPlugins {
            typer.context.withMode(ContextMode.ReTyping) {
              typer.typed(cpsTree, Mode.EXPRmode)
            }
          }
      }
      typedCpsTree
    }

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
      @${definitions.ScalaInlineClass} def $whileName[$domainName]($endWhileName: () => $domainName)(
        $bodyName: (() => $domainName) => $domainName,
        $conditionName: (_root_.scala.Boolean => $domainName) => $domainName): $domainName = {
        $conditionName { $conditionValueName: ${TypeTree()} =>
          if ($conditionValueName) {
            $bodyName { () =>
              $whileName[$domainName]($endWhileName)($bodyName, $conditionName)
            }
          } else {
            $endWhileName()
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
      @${definitions.ScalaInlineClass} def $doWhileName[$domainName]($endDoWhileName: () => $domainName)(
        $bodyName: (() => $domainName) => $domainName,
        $conditionName: (_root_.scala.Boolean => $domainName) => $domainName): $domainName = {
        $bodyName { () =>
          $conditionName { $conditionValueName: ${TypeTree()} =>
            if ($conditionValueName) {
              $doWhileName[$domainName]($endDoWhileName)($bodyName, $conditionName)
            } else {
              $endDoWhileName()
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

    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
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
                if (tpt.tpe.hasAnnotation(resetSymbol)) {
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
            val ifResultName = currentUnit.freshTermName("ifResult")

            q"""
            @${definitions.ScalaInlineClass} val $endIfName = { ($ifResultName: $tpe) => 
              ${continue(q"$ifResultName")}
            }
            ${cpsAttachment(cond) { condValue =>
              atPos(tree.pos) {
                q"""
                if ($condValue) ${cpsAttachment(thenp) { result =>
                  q"$endIfName($result)"
                }} else ${cpsAttachment(elsep) { result =>
                  q"$endIfName($result)"
                }}
                """
              }
            }}
            """
          case Match(selector, cases) =>
            val endMatchName = currentUnit.freshTermName("endMatch")
            val matchResultName = currentUnit.freshTermName("matchResult")
            val endMatchBody = continue(q"$matchResultName")

            q"""
            @${definitions.ScalaInlineClass} def $endMatchName($matchResultName: $tpe) = $endMatchBody
            ${cpsAttachment(selector) { selectorValue =>
              atPos(tree.pos) {
                Match(
                  selectorValue,
                  cases.map {
                    case caseDef @ CaseDef(pat, guard, body) =>
                      treeCopy.CaseDef(caseDef, pat, guard, cpsAttachment(body) { bodyValue =>
                        q"$endMatchName($bodyValue)"
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
            val continueName = currentUnit.freshTermName("continue")
            val unhandledExceptionName = currentUnit.freshTermName("unhandledException")
            val asCatcherName = currentUnit.freshTermName("asCatcher")

            q"""
            @${definitions.ScalaInlineClass} val $finalizerName = { ($tryResultName: _root_.scala.util.Try[$tpe]) => ${{
              cpsAttachment(finalizer) { finalizerValue =>
                q"""
                  ..${notPure(finalizerValue)}
                  ${continue(q"$tryResultName.get")}
                """
              }
            }}}

            @${definitions.ScalaInlineClass} def $asCatcherName[A](c: _root_.scala.util.control.Exception.Catcher[A]) = c

            $asCatcherName {
              case ..${{
              catches.map { caseDef =>
                atPos(caseDef.pos) {
                  CaseDef(caseDef.pat, caseDef.guard, cpsAttachment(caseDef.body) { bodyValue =>
                    q"$finalizerName(_root_.scala.util.Success($bodyValue))"
                  })
                }
              }
            }}
              case $unhandledExceptionName: _root_.scala.Throwable =>
                $finalizerName(_root_.scala.util.Failure($unhandledExceptionName))
            }.cpsCatch { $continueName: ${TypeTree()} => ${{
              cpsAttachment(block) { blockValue =>
                q"""
                val $tryResultName = $blockValue
                $continueName(${continue(q"$tryResultName")})
                """
              }
            }}}
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
            $whileName({ () =>
              ${continue(q"()")}
            })({ $continueName: ${TypeTree()} => ${cpsAttachment(body) { bodyValue =>
              q"""
                ..${notPure(bodyValue)}
                $continueName()
              """
            }}},
            { $conditionHandlerName: ${TypeTree()} => ${cpsAttachment(condition) { conditionValue =>
              q"$conditionHandlerName($conditionValue)"
            }}})
            """
          case q"do $body while($condition)" =>
            // TODO: Trampoline
            val continueName = currentUnit.freshTermName("continue")
            val conditionHandlerName = currentUnit.freshTermName("conditionHandler")
            q"""
            $doWhileDef

            $doWhileName({ () =>
              ${continue(q"()")}
            })({ $continueName: ${TypeTree()} => ${cpsAttachment(body) { bodyValue =>
              q"""
                ..${notPure(bodyValue)}
                $continueName()
              """
            }}},
            { $conditionHandlerName: ${TypeTree()} => ${cpsAttachment(condition) { conditionValue =>
              q"$conditionHandlerName($conditionValue)"
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
          val attachment: CpsAttachment = { continue: (Tree => Tree) =>
            val aName = currentUnit.freshTermName("a")

            // FIXME: tpe is a by-name type. I don't know why.
            atPos(tree.pos) {
              q"""
                $shiftOps.cpsApply { $aName: $tpe =>
                  ${continue(q"$aName")}
                }
              """
            }
          }
          tree.updateAttachment[CpsAttachment](attachment)
        } else if (isCpsTree(tree)) {
          tree.updateAttachment[CpsAttachment](cps)
        }
      }
      tpe
    }

    override def isActive(): Boolean = {
      active && phase.id < currentRun.picklerPhase.id
    }

    override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = {
      super.pluginsPt(pt, typer, tree, mode)
    }
  }

  val name: String = "dsl"

  private def resetMarker: PluginComponent = new PluginComponent with Transform {

    val global: CompilerPlugin.this.global.type = CompilerPlugin.this.global
    val phaseName: String = "resetmarker"
    val runsAfter = "parser" :: Nil
    override val runsBefore = "namer" :: Nil

    protected def newTransformer(unit: CompilationUnit): Transformer = new Transformer {

      private def annotateAsReset(tree: Tree) = {
        Annotated(q"new $resetSymbol()", transform(tree))
      }

      private def transformRootValDef(tree: ValDef) = {
        val ValDef(mods, name, tpt, rhs) = tree
        treeCopy.ValDef(tree, mods, name, tpt, annotateAsReset(rhs))
      }

      override def transformTemplate(tree: Template): Template = {
        val Template(parents, self, body) = tree
        treeCopy.Template(
          tree,
          parents,
          self,
          body.mapConserve {
            case valDef: ValDef =>
              transformRootValDef(valDef)
            case initializer: TermTree =>
              annotateAsReset(initializer)
            case stat =>
              super.transform(stat)
          }
        )
      }

      override def transform(tree: global.Tree): global.Tree = {
        tree match {
          case tree: TypeTree =>
            tree
          case Typed(expr, tpt) =>
            treeCopy.Typed(tree, transform(expr), tpt)
          case Function(vparams, body) =>
            treeCopy.Function(tree, vparams, annotateAsReset(body))
          case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            treeCopy.DefDef(tree, mods, name, tparams, transformValDefss(vparamss), tpt, annotateAsReset(rhs))
          case valDef: ValDef if valDef.mods.hasDefault =>
            transformRootValDef(valDef)
          case _ =>
            super.transform(tree)
        }
      }
    }
  }

  override val optionsHelp = Some(
    """This DSL plug-in accept only one flag `-P:dsl:macro-annotation-workaround`, which make this plug-in work in macro annotation generated code.""")

  override def init(options: List[String], error: String => Unit): Boolean = {
    options match {
      case Nil =>
        global.analyzer.addAnalyzerPlugin(cpsAnalyzerPlugin)
      case List("macro-annotation-workaround") =>
        global.analyzer.addAnalyzerPlugin(macroAnnotationWorkaroundAnalyzerPlugin)
        global.analyzer.addAnalyzerPlugin(cpsAnalyzerPlugin)
      case _ =>
        error(this.optionsHelp.get)
    }
    true
  }

  val description: String =
    "A compiler plugin that converts native imperative syntax to monadic expressions or continuation-passing style expressions"

  val components: List[PluginComponent] = {
    if (options.contains("macro-annotation-workaround")) {
      Nil
    } else {
      List(resetMarker)
    }
  }

  private val macroAnnotationWorkaroundAnalyzerPlugin = new AnalyzerPlugin {
    object Reset
    override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = {
      if (mode.inExprMode) {
        tree match {
          case function: Function =>
            // FIXME: the Reset attachment will be discarded when the body tree is replaced
            // (e.g. the body tree is a `+=` call, which will be replaced to an Assign tree
            function.body.updateAttachment(Reset)
          case defDef: DefDef =>
            defDef.rhs.updateAttachment(Reset)
            defDef.vparamss.foreach(_.foreach { _.rhs.updateAttachment(Reset) })
          case implDef: ImplDef =>
            implDef.impl.body.foreach {
              case valDef: ValDef =>
                valDef.rhs.updateAttachment(Reset)
              case termTree: TermTree =>
                termTree.updateAttachment(Reset)
              case _ =>
            }
          case _ =>
        }
      }
      pt
    }
    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      tree.attachments.get[Reset.type] match {
        case None =>
          tpe
        case Some(_) =>
          tpe.withAnnotations(List(Annotation(deactAnalyzerPlugins {
            typer.context.withMode(ContextMode.NOmode) {
              typer.typed(q"new $resetSymbol()", Mode.EXPRmode)
            }
          })))
      }
    }

    override def isActive(): Boolean = {
      active && phase.id < currentRun.picklerPhase.id
    }
  }

}
