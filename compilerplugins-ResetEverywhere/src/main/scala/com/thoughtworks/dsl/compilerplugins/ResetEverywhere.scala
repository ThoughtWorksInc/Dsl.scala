package com.thoughtworks.dsl.compilerplugins

import com.thoughtworks.dsl.Dsl.{ResetAnnotation, nonTypeConstraintReset, shift}

import scala.reflect.internal.FatalError
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
  * @note Once this [[ResetEverywhere]] plug-in is enabled,
  *       the `@[[Dsl.reset reset]]` annotations are added to class fields, every methods and every functions automatically.
  *       Some other macros or compiler plug-ins may conflict with those `@[[Dsl.reset reset]]` annotations.
  *
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

  private var nonTypeConstraintResetSymbol: Symbol = _ // = symbolOf[nonTypeConstraintReset]

  override def init(options: List[String], error: String => Unit): Boolean = {
    super.init(options, error) && {
      try {
        nonTypeConstraintResetSymbol = symbolOf[nonTypeConstraintReset]
        true
      } catch {
        case e: ScalaReflectionException =>
          error("""This compiler plug-in requires the runtime library:
  libraryDependencies += "com.thoughtworks.dsl" %% "dsl" % "latest.release"
""")
          false
      }
    }
  }
  val name: String = "ResetEverywhere"

  private final class ResetAnnotationCreator extends PluginComponent with Transform {

    val global: ResetEverywhere.this.global.type = ResetEverywhere.this.global
    val phaseName: String = "ResetAnnotationCreator"
    val runsAfter = "parser" :: Nil
    override val runsBefore = "namer" :: Nil

    protected def newTransformer(unit: CompilationUnit): Transformer = new Transformer {

      private def annotatedReset(tree: Tree) = {
        if (tree.isEmpty) {
          tree
        } else {
          Annotated(q"new $nonTypeConstraintResetSymbol()", transform(tree))
        }
      }

      private def typedReset(tree: Tree, typeTree: Tree) = {
        if (tree.isEmpty) {
          tree
        } else if (typeTree.isEmpty) {
          Annotated(q"new $nonTypeConstraintResetSymbol()", transform(tree))
        } else {
          Annotated(q"new $nonTypeConstraintResetSymbol()", Typed(transform(tree), typeTree))
        }
      }

      private def transformRootValDef(tree: ValDef) = {
        val ValDef(mods, name, tpt, rhs) = tree
        treeCopy.ValDef(tree, mods, name, tpt, typedReset(rhs, tpt))
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
            case defDef: DefDef if defDef.mods.hasFlag(Flag.MACRO) =>
              defDef
            case initializer: TermTree =>
              annotatedReset(initializer)
            case stat =>
              transform(stat)
          }
        )
      }

      private def annotateArgsAsReset(tree: Tree): Tree = {
        tree match {
          case tree: Apply =>
            treeCopy.Apply(tree, annotateArgsAsReset(tree.fun), tree.args.mapConserve(annotatedReset))
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
            treeCopy.Function(tree, vparams, annotatedReset(body))
          case DefDef(mods, name, tparams, vparamss, tpt, rhs)
              if name != termNames.CONSTRUCTOR && name != termNames.MIXIN_CONSTRUCTOR && rhs.nonEmpty && !mods
                .hasAnnotationNamed(definitions.TailrecClass.name) =>
            treeCopy.DefDef(tree, mods, name, tparams, transformValDefss(vparamss), tpt, typedReset(rhs, tpt))
          case valDef: ValDef if valDef.mods.hasDefault =>
            transformRootValDef(valDef)
          case Match(EmptyTree, cases) =>
            treeCopy.Match(
              tree,
              EmptyTree,
              cases.mapConserve { case caseDef @ CaseDef(pat, guard, body) =>
                treeCopy.CaseDef(caseDef, pat, guard, annotatedReset(body))
              }
            )
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
