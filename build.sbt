
//Project settings
lazy val mainSettings = Seq(
  name := "Minions",
  version := "1.0",
  scalaVersion := "2.12.2"
)

//Extra scala compiler options
lazy val extraCompilerOptions = Seq(
  "-deprecation",
  "-feature",
  "-language:existentials",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint:-unused,_",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-infer-any",
  "-Ywarn-nullary-unit",
  "-Ywarn-inaccessible",
  "-Ywarn-value-discard",
  "-Ywarn-unused:params",
  "-Ywarn-unused:patvars",
  "-Ywarn-unused:privates",
  "-Ywarn-unused:implicits",
  "-Ywarn-unused:locals",
  "-Xfuture"
)

//This option is actually irrelevant for us because we're using a browser to run and test the client, rather
//than writing something that should run like a server on Node.js or a similar javascript runtime engine.
//Disables Rhino and makes ScalaJS use Node.js instead
//Rhino is slow, but it doesn't require separate installation should work out-of-the-box
//Node.js is fast, but needs separate installation
//scalaJSUseRhino in Global := false

//Helpful docs for the below setup
//https://www.scala-js.org/tutorial/basic/
//https://www.scala-js.org/doc/project/cross-build.html
//https://www.scala-js.org/api/sbt-scalajs/0.6.10/#org.scalajs.sbtplugin.cross.CrossProject

//Define a type of project where we have shared scala files (in core/),
//scala files that are compiled for the jvm only (in server/),
//and scala file that are compiled for javascript only (in client/)
lazy val scalaJSCrossType = new org.scalajs.sbtplugin.cross.CrossType() {
  override def sharedSrcDir(projectBase: File, conf: String): Option[File] = {
    conf match {
      case "main" => Some(new File(projectBase.getParentFile(),"core/src/main/scala"))
      case "test" => Some(new File(projectBase.getParentFile(),"core/src/test/scala"))
    }
  }
  override def projectDir(crossBase: File, projectType: String): File = {
    projectType match {
      case "jvm" => new File(crossBase,"server")
      case "js" => new File(crossBase,"client")
      case _ => throw new Exception("Invalid projectType: " + projectType)
    }
  }
}

//Create the project
lazy val minions = crossProject.crossType(scalaJSCrossType).in(file(".")).
  settings(mainSettings: _*).
  settings(
    scalacOptions ++= extraCompilerOptions,
    libraryDependencies ++= Seq (
      "com.typesafe.play" %%% "play-json" % "2.6.3"
    )
  ).
  jvmSettings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % "10.0.10"
    )
  ).
  jsSettings(
    name := "MinionsClient",
    //Create a javascript launcher to call the main class for us
    persistLauncher in Compile := true,
    persistLauncher in Test := false,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.1",
      "be.doeraene" %%% "scalajs-jquery" % "0.9.1"
    )
  )

//Pull out the subprojects so that sbt knows they're there
//Note - do NOT modify the project settings here, instead use jvmSettings and jsSettings above
lazy val minionsServer = minions.jvm
lazy val minionsClient = minions.js

//Custom task that copies necessary stuff to web/ directory
val copyStuffTask = taskKey[Unit]("Copy to web directory")
copyStuffTask := {
  val unused = (fastOptJS in Compile in minionsClient).value
  streams.value.log.info("Copying stuff to web/")
  def createDirectory(dst: String) = IO.createDirectory(new File(dst))
  def copyFile(src: String, dst: String) = IO.copyFile(new File(src), new File(dst))
  createDirectory("web")
  createDirectory("web/js")
  copyFile("client/minionsclient_dev.html","web/index.html")
  copyFile("client/jquery-2.1.1.min.js","web/js/jquery-2.1.1.min.js")
  copyFile("client/target/scala-2.12/minionsclient-fastopt.js","web/js/minionsclient-fastopt.js")
  copyFile("client/target/scala-2.12/minionsclient-fastopt.js.map","web/js/minionsclient-fastopt.js.map")
  copyFile("client/target/scala-2.12/minionsclient-jsdeps.js","web/js/minionsclient-jsdeps.js")
  copyFile("client/target/scala-2.12/minionsclient-launcher.js","web/js/minionsclient-launcher.js")
}

//Custom alias that builds everything and copies it
addCommandAlias("buildEverything", ";project minions;compile;fastOptJS;copyStuffTask")
