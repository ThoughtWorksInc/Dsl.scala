package com.thoughtworks.dsl.delimitedcontinuation

import com.thoughtworks.dsl.delimitedcontinuation.annotations.{reset, shift}

import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.typechecker.ContextMode
import scala.tools.nsc.{Global, Mode}
private object CompilerPlugin {
  private[CompilerPlugin] object Reset
}

/**
  * @author 杨博 (Yang Bo)
  */
final class CompilerPlugin(override val global: Global) extends Plugin {
  import CompilerPlugin._
  import global._
  import global.analyzer._

  private type CpsAttachment = (Tree => Tree) => Tree

  val name: String = "delimitedcontinuation"

  val components: List[PluginComponent] = Nil

  val description: String =
    "A compiler plugin that converts native imperative syntax to monadic expressions or continuation-passing style expressions"

  private val analyzerPlugin: AnalyzerPlugin = new AnalyzerPlugin {
    private val resetSymbol = symbolOf[reset]
    private val shiftSymbol = symbolOf[shift]

    override def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = {
      mode.inExprMode && tree.tpe.hasAnnotation(resetSymbol)
    }

    override def adaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Tree = {
      val Some(attachment) = tree.attachments.get[CpsAttachment]
      val Seq(typedCpsTree) = tree.tpe.annotations.collect {
        case annotation if annotation.matches(resetSymbol) =>
          val cpsTree = resetAttrs(attachment(identity))
//          reporter.info(tree.pos, s"Translating to continuation-passing style: $cpsTree", true)
          deact {
            typer.context.withMode(ContextMode.ReTyping) {
              typer.typed(cpsTree, Mode.EXPRmode)
            }
          }
      }
      typedCpsTree
    }

    override def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = {
      if (mode.inExprMode) {
        tree match {
          case function: Function =>
            function.body.updateAttachment(Reset)
          case defDef: DefDef =>
            defDef.rhs.updateAttachment(Reset)
          case implDef: ImplDef =>
            implDef.impl.body.foreach(_.updateAttachment(Reset))
          case _ =>
        }
      }
      pt
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

    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      def cps(continue: Tree => Tree): Tree = atPos(tree.pos) {
        tree match {
          case q"$prefix.$method[..$typeParameters](...$parameterLists)" =>
            cpsAttachment(prefix) { prefixValue =>
              cpsParameterList(parameterLists) { parameterListsValues =>
                atPos(tree.pos) {
                  q"$prefixValue.$method[..$typeParameters](...$parameterListsValues)"
                }
              }
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
                continue(treeCopy.Typed(tree, exprValue, tpt))
              }
            }
          case Block(stats, expr) =>
            def loop(stats: List[Tree]): Tree = {
              stats match {
                case Nil =>
                  cpsAttachment(expr)(continue)
                case head :: tail =>
                  def notPure(head: Tree): List[Tree] = {
                    if (head.isInstanceOf[Ident]) {
                      Nil
                    } else {
                      head :: Nil
                    }
                  }
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
            println("CasDef")
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
                  $finalizerValue
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
        }
      }
      def checkResetAttachment: Type = {
        tree.attachments.get[Reset.type] match {
          case None =>
            tpe
          case Some(_) =>
            tpe.withAnnotations(List(Annotation(deact {
              typer.context.withMode(ContextMode.NOmode) {
                typer.typed(q"new $resetSymbol()", Mode.EXPRmode)
              }
            })))
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
          checkResetAttachment
        } else if (isCpsTree(tree)) {
          tree.updateAttachment[CpsAttachment](cps)
          checkResetAttachment
        } else {
          tpe
        }

      } else {
        tpe
      }
    }

    private var active = true
    private def deact[A](run: => A): A = {
      synchronized {
        active = false
        try {
          run
        } finally {
          active = true
        }
      }
    }

    override def isActive(): Boolean = {
      active && phase.id < currentRun.picklerPhase.id
    }

  }

  global.analyzer.addAnalyzerPlugin(analyzerPlugin)

}
