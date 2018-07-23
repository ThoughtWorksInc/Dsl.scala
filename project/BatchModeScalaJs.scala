import sbt.{AutoPlugin, Def}
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

/**
  * @author 杨博 (Yang Bo)
  */
object BatchModeScalaJs extends AutoPlugin {

  override def requires = ScalaJSPlugin

  override def projectSettings: Seq[Def.Setting[_]] = Seq(
    scalaJSLinkerConfig := {
      scalaJSLinkerConfig.value.withBatchMode(true)
    }
  )
  override def trigger = allRequirements

}
