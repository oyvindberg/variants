// Scala.meta macros are available for two most recent minor versions of Scala.
// At the time of writing, that's 2.11.11 and 2.12.2.
scalaVersion in ThisBuild := "2.12.2"

lazy val metaMacroSettings: Seq[Def.Setting[_]] = Seq(
  // New-style macro annotations are under active development.  As a result, in
  // this build we'll be referring to snapshot versions of both scala.meta and
  // macro paradise.
  resolvers += Resolver.defaultLocal,
  resolvers += Resolver.sonatypeRepo("releases"),
  resolvers += Resolver.bintrayRepo("scalameta", "maven"),
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
//  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full),
  addCompilerPlugin("org.scalameta" % "paradise_2.12.2" % "3.0.0-315-91b58ff4"),

  scalacOptions += "-Xplugin-require:macroparadise",
//  scalacOptions += "-Xplugin-list",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in (Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in (Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)

lazy val runtime = project

// Define macros in this project.
lazy val macros = project.settings(
  metaMacroSettings,
  // A dependency on scala.meta is required to write new-style macros, but not
  // to expand such macros.  This is similar to how it works for old-style
  // macros and a dependency on scala.reflect.
  libraryDependencies ++= Seq(
    "org.scalameta" %% "scalameta" % "1.8.0",
    "org.scalameta" %% "testkit" % "1.8.0" % Test,
    "com.lihaoyi" %% "utest" % "0.4.7" % Test
  ),
  testFrameworks += new TestFramework("utest.runner.Framework")
).dependsOn(runtime)

// Use macros in this project.
lazy val app = project.settings(metaMacroSettings).dependsOn(macros % Provided, runtime)