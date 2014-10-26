package org.jetbrains.plugins.scala
package project

import com.intellij.openapi.components._
import com.intellij.openapi.components.StoragePathMacros._
import com.intellij.openapi.project.Project

/**
 * @author Pavel Fatin
 */
@State(name = "ScalaCompilerConfiguration", storages = Array (
  new Storage(file = PROJECT_FILE),
  new Storage(file = PROJECT_CONFIG_DIR + "/scala_compiler.xml", scheme = StorageScheme.DIRECTORY_BASED)))
class ScalaCompilerSettings extends PersistentStateComponent[ScalaCompilerSettingsState]{
  var incrementalityType: IncrementalityType = _
  var compileOrder: CompileOrder = _

  var dynamics: Boolean = _
  var postfixOps: Boolean = _
  var reflectiveCalls: Boolean = _
  var implicitConversions: Boolean = _
  var higherKinds: Boolean = _
  var existentials: Boolean = _
  var macros: Boolean = _

  var warnings: Boolean = _
  var deprecationWarnings: Boolean = _
  var uncheckedWarnings: Boolean = _
  var featureWarnings: Boolean = _
  var optimiseBytecode: Boolean = _
  var explainTypeErrors: Boolean = _
  var specialization: Boolean = _
  var continuations: Boolean = _

  var debuggingInfoLevel: DebuggingInfoLevel = _
  var additionalCompilerOptions: Seq[String] = _
  var plugins: Seq[String] = _

  loadState(new ScalaCompilerSettingsState())

  private val ToggleOptions: Seq[(String, () => Boolean, Boolean => Unit)] = Seq(
    ("-language:dynamics", () => dynamics, dynamics = _),
    ("-language:postfixOps", () => postfixOps, postfixOps = _),
    ("-language:reflectiveCalls", () => reflectiveCalls, reflectiveCalls = _),
    ("-language:implicitConversions", () => implicitConversions, implicitConversions = _),
    ("-language:higherKinds", () => higherKinds, higherKinds = _),
    ("-language:existentials", () => existentials, existentials = _),
    ("-language:macros", () => macros, macros = _),
    ("-nowarn", () => warnings, (b: Boolean) => warnings = !b),
    ("-deprecation", () => deprecationWarnings, deprecationWarnings = _),
    ("-unchecked", () => uncheckedWarnings, uncheckedWarnings = _),
    ("-feature", () => featureWarnings, featureWarnings = _),
    ("-optimise", () => optimiseBytecode, optimiseBytecode = _),
    ("-explaintypes", () => explainTypeErrors, explainTypeErrors = _),
    ("-no-specialization", () => specialization, (b: Boolean) => specialization = !b),
    ("-P:continuations:enable", () => continuations, continuations = _))

  private val DebuggingOptions: Map[String, DebuggingInfoLevel] = Map(
    "-g:none" -> DebuggingInfoLevel.None,
    "-g:source" -> DebuggingInfoLevel.Source,
    "-g:line" -> DebuggingInfoLevel.Line,
    "-g:vars" -> DebuggingInfoLevel.Vars,
    "-g:notc" -> DebuggingInfoLevel.Notc)

  private val PluginOptionPattern = "-P:(.+)".r

  def toOptions: Seq[String] = {
    val debuggingLevelToOption = DebuggingOptions.map(_.swap)

    val toggledOptions = ToggleOptions.collect {
      case (option, getter, _) if getter() => option
    }

    val debuggingLevelOption = debuggingLevelToOption(debuggingInfoLevel)
    
    val pluginOptions = plugins.map(path => "-P:" + path)

    (toggledOptions :+ debuggingLevelOption) ++ pluginOptions
  }

  def configureFrom(options: Seq[String]) {
    val optionToSetter = ToggleOptions.map(it => (it._1, it._3)).toMap

    optionToSetter.foreach {
      case (option, setter) => setter(options.contains(option))
    }

    debuggingInfoLevel = DebuggingOptions.find(p => options.contains(p._1)).map(_._2).getOrElse(DebuggingInfoLevel.Vars)

    plugins = options collect {
      case PluginOptionPattern(path) => path
    }

    additionalCompilerOptions = options.filterNot { option =>
      optionToSetter.keySet.contains(option) ||
              DebuggingOptions.keySet.contains(option) ||
              PluginOptionPattern.findFirstIn(option).isDefined
    }
  }

  def loadState(state: ScalaCompilerSettingsState) {
    incrementalityType = state.incrementalityType
    compileOrder = state.compileOrder

    dynamics = state.dynamics
    postfixOps = state.postfixOps
    reflectiveCalls = state.reflectiveCalls
    implicitConversions = state.implicitConversions
    higherKinds = state.higherKinds
    existentials = state.existentials
    macros = state.macros

    warnings = state.warnings
    deprecationWarnings = state.deprecationWarnings
    uncheckedWarnings = state.uncheckedWarnings
    featureWarnings = state.featureWarnings
    optimiseBytecode = state.optimiseBytecode
    explainTypeErrors = state.explainTypeErrors
    specialization = state.specialization
    continuations = state.continuations

    debuggingInfoLevel = state.debuggingInfoLevel
    additionalCompilerOptions = state.additionalCompilerOptions.toSeq
    plugins = state.plugins.toSeq
  }
  
  def getState = {
    val state = new ScalaCompilerSettingsState()
    state.incrementalityType = incrementalityType
    state.compileOrder = compileOrder

    state.dynamics = dynamics
    state.postfixOps = postfixOps
    state.reflectiveCalls = reflectiveCalls
    state.implicitConversions = implicitConversions
    state.higherKinds = higherKinds
    state.existentials = existentials
    state.macros = macros

    state.warnings = warnings
    state.deprecationWarnings = deprecationWarnings
    state.uncheckedWarnings = uncheckedWarnings
    state.featureWarnings = featureWarnings
    state.optimiseBytecode = optimiseBytecode
    state.explainTypeErrors = explainTypeErrors
    state.specialization = specialization
    state.continuations = continuations

    state.debuggingInfoLevel = debuggingInfoLevel
    state.additionalCompilerOptions = additionalCompilerOptions.toArray
    state.plugins = plugins.toArray
    state
  }
}

object ScalaCompilerSettings {
  def instanceIn(project: Project): ScalaCompilerSettings =
    ServiceManager.getService(project, classOf[ScalaCompilerSettings])
}
