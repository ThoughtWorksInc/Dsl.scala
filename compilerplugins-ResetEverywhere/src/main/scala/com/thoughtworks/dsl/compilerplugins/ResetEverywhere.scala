package com.thoughtworks.dsl.compilerplugins

import com.thoughtworks.dsl.Dsl.{ResetAnnotation, nonTypeConstraintReset, shift}

import scala.tools.nsc.plugins.{Plugin, PluginComponent}
import scala.tools.nsc.transform.Transform
import scala.tools.nsc.typechecker.ContextMode
import scala.tools.nsc.{Global, Mode, Phase}

/** A Scala compiler plug-in to enable [[Dsl.Keyword#unary_$bang !-notation]] for every methods and functions.
  *
  * Add the following setting in your `build.sbt` to enable this plug-in.
  *
  * `<pre>
  * // build.sbt
  * addCompilerPlugin("com.thoughtworks.dsl" %% "compilerplugins-reseteverywhere" % "latest.release")
  * </pre>`
  *
  * @note All `@[[Dsl.reset reset]]` annotations are not neseccary if this [[ResetEverywhere]] plug-in is enabled.
  * @author 杨博 (Yang Bo)
  */
final class ResetEverywhere(override val global: Global) extends Plugin {
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

  val name: String = "ResetEverywhere"

  private final class ResetAnnotationCreator extends PluginComponent with Transform {

    val global: ResetEverywhere.this.global.type = ResetEverywhere.this.global
    val phaseName: String = "ResetAnnotationCreator"
    val runsAfter = "parser" :: Nil
    override val runsBefore = "namer" :: Nil

    protected def newTransformer(unit: CompilationUnit): Transformer = new Transformer {

      private def annotateAsReset(tree: Tree) = {
        if (tree.isEmpty) {
          tree
        } else {
          Annotated(q"new $nonTypeConstraintResetSymbol()", transform(tree))
        }
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
            case valDef: ValDef if !valDef.mods.isParamAccessor =>
              transformRootValDef(valDef)
            case initializer: TermTree =>
              annotateAsReset(initializer)
            case stat =>
              transform(stat)
          }
        )
      }

      private def annotateArgsAsReset(tree: Tree): Tree = {
        tree match {
          case tree: Apply =>
            treeCopy.Apply(tree, annotateArgsAsReset(tree.fun), tree.args.mapConserve(annotateAsReset))
          case fun =>
            fun
        }
      }

      override def transform(tree: global.Tree): global.Tree = {
        tree match {
          case tree: TypeTree =>
            tree
          case Typed(expr, tpt) =>
            treeCopy.Typed(tree, transform(expr), tpt)
          case Function(vparams, body) =>
            treeCopy.Function(tree, vparams, annotateAsReset(body))
          case DefDef(mods, name, tparams, vparamss, tpt, rhs)
              if name != termNames.CONSTRUCTOR && name != termNames.MIXIN_CONSTRUCTOR && rhs.nonEmpty && !mods
                .hasAnnotationNamed(definitions.TailrecClass.name) =>
            treeCopy.DefDef(tree, mods, name, tparams, transformValDefss(vparamss), tpt, annotateAsReset(rhs))
          case valDef: ValDef if valDef.mods.hasDefault =>
            transformRootValDef(valDef)
          case Match(EmptyTree, cases) =>
            treeCopy.Match(tree, EmptyTree, cases.mapConserve {
              case caseDef @ CaseDef(pat, guard, body) =>
                treeCopy.CaseDef(caseDef, pat, guard, annotateAsReset(body))
            })
          case q"${Ident(termNames.CONSTRUCTOR)}(...$argss)" =>
            annotateArgsAsReset(tree)
          case q"super.${termNames.CONSTRUCTOR}(...$argss)" =>
            annotateArgsAsReset(tree)
          case _ =>
            super.transform(tree)
        }
      }
    }
  }

  val description: String = "Add @reset annotation for every methods and functions automatically"

  val components: List[PluginComponent] = List(new ResetAnnotationCreator)

}
