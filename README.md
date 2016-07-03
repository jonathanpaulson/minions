# minions

## Getting started

### Setup
1. Install [sbt](http://www.scala-sbt.org/download.html)
2. Clone this github repo to any desired directory and navigate to that directory.
3. Run `sbt`.
4. Within sbt, run `compile` to build the Scala code.
5. Within sbt, run `core/run`, or `server/run`, or `client/run` to run the main program for each of these three subprojects.

### Project Organization
The code consists of three subprojects: core, server, client, each one with its own source directory, structured in the usual way for an sbt project.

At the moment, server and client both depend on core but not one another. Client also uses ScalaJS and compiles into Javascript.

See build.sbt for all the nitty-gritty details of the configuration.

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
