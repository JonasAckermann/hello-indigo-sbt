
lazy val helloindigo =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin, SbtIndigo)
    .settings( // Normal SBT settings
      name := "helloindigo",
      version := "0.0.1",
      scalaVersion := "2.13.2",
      organization := "helloindigo",
      libraryDependencies ++= Seq(
        "com.lihaoyi"    %%% "utest"      % "0.7.4"  % "test",
        "org.scalacheck" %%% "scalacheck" % "1.14.3" % "test"
      ),
      testFrameworks += new TestFramework("utest.runner.Framework")
    )
    .settings( // Indigo specific settings
      showCursor := true,
      title := "Hello, Indigo! - Made with Indigo",
      gameAssetsDirectory := "assets",
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "indigo-json-circe" % "0.1.0",
        "io.indigoengine" %%% "indigo"            % "0.1.0",
        "io.indigoengine" %%% "indigo-extras"     % "0.1.0"
      )
    )

addCommandAlias("buildGame", ";compile;fastOptJS;indigoBuildJS")
addCommandAlias("publishGame", ";compile;fullOptJS;indigoPublishJS")
