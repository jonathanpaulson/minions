
//See project/plugins.sbt
//enablePlugins(ScalaJSPlugin)

//Specifies that there is a project called 'root' that lives in directory "."
lazy val root = (project in file(".")).
  settings(
    name := "Minions",
    version := "1.0",
    scalaVersion := "2.11.8",

    scalacOptions ++= Seq(
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

  )
