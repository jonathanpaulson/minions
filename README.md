# minions

## Getting Started

### Instructions
1. Install [sbt](http://www.scala-sbt.org/download.html), the scala build tool.
2. Clone this github repo to any desired directory and navigate to that directory.
3. Run `sbt`.
4. Within sbt, run `compile` to compile the server.
5. Within sbt, run `fastOptJS` to compile the client via ScalaJS into Javascript.
6. Within sbt, you can now run `MinionsJVM/run` to run the server.
7. In a browser, you can now open client/minionsclient_dev.html to run the dev client.

Run `package` if you would like to generate a standalone executable JAR file for the server.
Run `fullOptJS` to build the production client. The only difference with fastOptJS is that it's slower to compile but optimizes more.
You can then run it via client/minionsclient.html.

### Project Organization
The code consists of two projects: server and client, each one with its own source directory, structured in the usual way for an sbt project.
Additionally, there is a source folder called 'core' that is shared between the two.

The main class and entry point for server is server/src/main/scala/ServerMain.scala
The main class and entry point for client is server/src/main/scala/ClientMain.scala

Server is compiled the usual way, to run on the JVM.
Client is compiled via ScalaJS into javascript.

See build.sbt for all the nitty-gritty details of the configuration.

## Troubleshooting

### SBT memory issues

Note that to avoid memory leaks via Java's permanent generation in a long-running sbt process,
you may need to edit your sbt configuration (i.e. the sbt script installed at ~/bin/sbt) if
you have Java 1.7 or earlier. If you do encounter out-of-memory issues in sbt, try editing the script
to call Java with the following flags:

    -XX:+CMSClassUnloadingEnabled
    -XX:+UseConcMarkSweepGC
    -XX:MaxPermSize=1G

### Scala SBT file name length errors

If during a compile in SBT you encounter the error `filename too long` or similar, it may be due to sbt trying to generate a file whose name exceeds the max allowed filename length on your system. See if you can specify an override for your sbt install to cap the filename length it uses:

http://stackoverflow.com/questions/28565837/filename-too-long-sbt

## Contributors

* Jonathan Paulson
* David Wu
