package com.thoughtworks.each

import scala.tools.nsc.{Global, Mode, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.typechecker.ContextMode
private object CompilerPlugin {
  private[CompilerPlugin] object Reset
}
import CompilerPlugin._

/**
  * @author 杨博 (Yang Bo)
  */
final class CompilerPlugin(override val global: Global) extends Plugin {
  import global._
  import global.analyzer._

  private type EachAttachment = (Tree => Tree) => Tree

  val name: String = "each"

  val components: List[PluginComponent] = Nil

  val description: String =
    "A compiler plugin that converts native imperative syntax to monadic expressions or continuation-passing style expressions"

  val analyzerPlugin: AnalyzerPlugin = new AnalyzerPlugin {
    private val resetSymbol = symbolOf[EachOps.reset]

    private val bangSymbol = typeOf[EachOps[Any]].member(TermName("!"))

    override def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = {
      tree.tpe.annotations.exists { annotation =>
        annotation.matches(resetSymbol)
      }
    }

    override def adaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Tree = {
      val Some(attachment) = tree.attachments.get[EachAttachment]
      val Seq(typedCpsTree) = tree.tpe.annotations.collect {
        case annotation if annotation.matches(resetSymbol) =>
          val cpsTree = attachment(identity)
          println("replacing " + tree + " to " + cpsTree)
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
          case _ =>
        }
      }
      pt
    }

    private def cps(tree: Tree)(continue: Tree => Tree): Tree = {
      tree.attachments.get[EachAttachment] match {
        case Some(attachment) => attachment(continue)
        case None             => continue(tree)
      }
    }
    private def cpsParameter(parameters: List[Tree])(continue: List[Tree] => Tree): Tree = {
      parameters match {
        case Nil =>
          continue(Nil)
        case head :: tail =>
          cps(head) { headValue =>
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
    override def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
      def attachmentOption: Option[EachAttachment] = tree match {
        case q"$eachOps.!" if eachOps.tpe <:< weakTypeOf[EachOps[Any]] =>
          Some[EachAttachment] { continue: (Tree => Tree) =>
            val aName = currentUnit.freshTermName("a")
            atPos(tree.pos) {
              q"""
                $eachOps { $aName: $tpe =>
                  ${continue(q"$aName")}
                }
              """
            }
          }
        case q"$prefix.$method[..$typeParameters](...$parameterLists)" =>
          if (prefix.hasAttachment[EachAttachment] ||
              parameterLists.exists(_.exists(_.hasAttachment[EachAttachment]))) {
            Some[EachAttachment] { continue =>
              cps(prefix) { prefixValue =>
                cpsParameterList(parameterLists) { parameterListsValues =>
                  atPos(tree.pos) {
                    q"$prefixValue.$method[..$typeParameters](...$parameterListsValues)"
                  }
                }
              }
            }
          } else {
            None
          }
        case Block(stats, expr) =>
          def loop(stats: List[Tree]): Option[EachAttachment] = {
            stats match {
              case Nil =>
                expr.attachments.get[EachAttachment]
              case head :: tail =>
                def notPure(head: Tree): List[Tree] = {
                  if (head.isInstanceOf[Ident]) {
                    Nil
                  } else {
                    head :: Nil
                  }
                }
                loop(tail) match {
                  case Some(tailContinuation) =>
                    head.attachments.get[EachAttachment] match {
                      case Some(headContinuation) =>
                        Some[EachAttachment] { (continue: Tree => Tree) =>
                          headContinuation { headValue =>
                            q"..${notPure(headValue)}; ${tailContinuation(continue)}"
                          }
                        }
                      case None =>
                        Some[EachAttachment] { (continue: Tree => Tree) =>
                          q"..${notPure(head)}; ${tailContinuation(continue)}"
                        }
                    }
                  case None =>
                    head.attachments.get[EachAttachment] match {
                      case Some(headContinuation) =>
                        Some[EachAttachment] { (continue: Tree => Tree) =>
                          headContinuation { headValue =>
                            Block(notPure(headValue) ::: tail, continue(expr))
                          }
                        }
                      case None =>
                        None
                    }
                }
            }
          }
          loop(stats)

        case If(cond, thenp, elsep) =>
          if (cond.hasAttachment[EachAttachment] || thenp.hasAttachment[EachAttachment] || elsep
                .hasAttachment[EachAttachment]) {
            Some[EachAttachment] { continue =>
              val endIfName = currentUnit.freshTermName("endIf")
              val ifResultName = currentUnit.freshTermName("ifResult")
              val endIfBody = continue(q"$ifResultName")
              atPos(tree.pos) {
                cps(cond) { condValue =>
                  q"""
                  @_root_.scala.inline def $endIfName($ifResultName: $tpe) = $endIfBody
                  if ($condValue) ${cps(thenp) { result =>
                    q"$endIfName($result)"
                  }} else ${cps(elsep) { result =>
                    q"$endIfName($result)"
                  }}
                """
                }
              }
            }

          } else {
            None
          }
        case _ =>
          None
      }

      if (mode.inExprMode) {
        attachmentOption match {
          case None =>
            tpe
          case Some(attachment) =>
            tree.updateAttachment(attachment)
            tree.attachments.get[Reset.type] match {
              case None =>
                tpe
              case Some(_) =>
                tpe.withAnnotations(List(Annotation(deact {
                  typer.context.withMode(ContextMode.NOmode) {
                    typer.typed(q"new _root_.com.thoughtworks.each.EachOps.reset()", Mode.EXPRmode)
                  }
                })))
            }
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
