ThisBuild / scalaVersion := "2.13.18"
ThisBuild / version := "0.1.0"
ThisBuild / organization := "Dumb-Projects-Inc"

val chiselVersion = "7.6.0"

lazy val isaSim = RootProject(
  uri("https://github.com/Dumb-Projects-Inc/02155-RISC-V-ISA-SIM.git")
)

lazy val root = (project in file("."))
  .settings(
    name := "RISC-V-Microprocessor",
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel" % chiselVersion,
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "com.carlosedp" %% "riscvassembler" % "1.10.0"
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
    ),
    Test / testOptions +=
      Tests.Argument(TestFrameworks.ScalaTest, "-DemitVcd=1")
  )
  .dependsOn(isaSim % "test->compile")
