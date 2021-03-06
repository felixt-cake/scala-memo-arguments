import Dependencies._


lazy val libDependencies = Seq(
  scalaMeta % Compile,
  scalaMetaContrib % Compile,
  // testing
  scalaTest % Test
)

lazy val metaMacroSettings = Seq(
  // A dependency on macro paradise 3.x is required to both write and expand
  // new-style macros.  This is similar to how it works for old-style macro
  // annotations and a dependency on macro paradise 2.x.
  addCompilerPlugin("org.scalameta" % "paradise" % "3.0.0-M8" cross CrossVersion.full),
  scalacOptions += "-Xplugin-require:macroparadise",
  // temporary workaround for https://github.com/scalameta/paradise/issues/10
  scalacOptions in(Compile, console) := Seq(), // macroparadise plugin doesn't work in repl yet.
  // temporary workaround for https://github.com/scalameta/paradise/issues/55
  sources in(Compile, doc) := Nil // macroparadise doesn't work with scaladoc yet.
)

lazy val root = (project in file(".")).
  settings(
    name := "scala-memo-arguments",
    description := "Memoized arguments in Scala",
    organization := "com.terkhorn",
    scalaVersion := "2.12.2",
    version := "0.1.0",
    crossScalaVersions := Seq("2.11.11", "2.12.2"),
    libraryDependencies ++= libDependencies,
    licenses := ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0")) :: Nil,
    metaMacroSettings,
    publishArtifact in Test := false,
    publishMavenStyle := false
  ).enablePlugins(ScalafmtPlugin)
