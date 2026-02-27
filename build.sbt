import sbtwelcome._

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val Lmalen_zahlen =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name         := "malen_zahlen",
      version      := "0.0.1",
      scalaVersion := "3.8.1",
      organization := "malen_zahlen",
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "tyrian-io"     % "0.14.0",
        "io.circe"        %%% "circe-core"    % "0.14.15",
        "io.circe"        %%% "circe-parser"  % "0.14.15",
        "io.circe"        %%% "circe-generic" % "0.14.15",
        "org.scalameta"   %%% "munit"         % "1.2.2" % Test
      ),
      testFrameworks += new TestFramework("munit.Framework"),
      scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
      Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "target" / "scalajs-dev",
      Compile / fullLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value / "target" / "scalajs-opt",
      scalafixOnCompile                                   := true,
      semanticdbEnabled                                   := true,
      semanticdbVersion                                   := scalafixSemanticdb.revision,
      autoAPIMappings                                     := true
    )
    .settings(
      logo := List(
        "",
        "Malen Zahlen (v" + version.value + ")",
        ""
      ).mkString("\n"),
      usefulTasks := Seq(
        UsefulTask("fastLinkJS", "Rebuild the JS (use during development)").noAlias,
        UsefulTask("fullLinkJS", "Rebuild the JS and optimise (use in production)").noAlias
      ),
      logoColor        := scala.Console.MAGENTA,
      aliasColor       := scala.Console.BLUE,
      commandColor     := scala.Console.CYAN,
      descriptionColor := scala.Console.WHITE
    )
