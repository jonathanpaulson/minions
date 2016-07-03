
//Common settings across all subprojects
lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.11.8"
)

//Extra scala compiler options
lazy val extraCompilerOptions = Seq(
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-Xfuture"
)

//"Core" project - contains main board and game implementation and shared logic
//Source is located in directory "core"
lazy val core = (project in file("core")).
  settings(commonSettings: _*).
  settings(
    name := "MinionsCore",
    scalacOptions ++= extraCompilerOptions
  )

//Server project - contains server-side code
//Source is located in directory "server"
lazy val server = (project in file("server")).
  settings(commonSettings: _*).
  settings(
    name := "MinionsServer",
    scalacOptions ++= extraCompilerOptions
  ).
  dependsOn(core)

//Client project - uses ScalaJS to run javascript
//Source is located in directory "client"
lazy val client = (project in file("client")).
  settings(commonSettings: _*).
  settings(
    name := "MinionsClient",
    scalacOptions ++= extraCompilerOptions
  ).
  enablePlugins(ScalaJSPlugin). //see project/plugins.sbt
  dependsOn(core)


//Root project encompassing all 3 subprojects
lazy val root = (project in file(".")).
  aggregate(core, server, client)
