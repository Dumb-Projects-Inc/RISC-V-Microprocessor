ThisBuild / scalaVersion := "2.13.18"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "Dumb-Projects-Inc"

val chiselVersion = "7.6.0"

lazy val root = (project in file("."))
  .settings(
    name := "RISC-V-Microprocessor",
    resolvers += "jitpack" at "https://jitpack.io",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "com.github.push-and-pray" %% "riscvassembler" % "jp-SNAPSHOT",
      "com.github.Dumb-Projects-Inc" %% "02155-RISC-V-ISA-SIM" % "40df390f7a"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations"
    ),
    addCompilerPlugin(
      "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
    )
  )

addCommandAlias("testVcd", "testOnly -- -DemitVcd=1")
