# minions

## Getting Started

### Instructions
1. Install [sbt](http://www.scala-sbt.org/download.html), the scala build tool.
2. Clone this github repo to any desired directory and navigate to that directory.
3. Run `sbt`.
4. Within sbt, run `buildEverything` to compile everything. This is equivalent to `compile` (build the server), `fastOptJS` (compile the client via ScalaJS to Javascript), and `copyStuffTask` (create a web/ folder and copy everything there, where the server expects it).
5. Outside of sbt, edit application.conf to have whatever settings you like.
6. Within sbt, you can now run `minionsJVM/run` to run the server.
7. In a browser, browse to http://localhost:8080/?username=<YOUR USERNAME HERE>&side=<0 OR 1>.

Run `package` if you would like to generate a standalone executable JAR file for the server.
Run `fullOptJS` to build the production client. The only difference with fastOptJS is that it's slower to compile but optimizes more. Note that there is no equivalent of copyStuffTask defined yet for this mode, you have to do things manually.

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

## Local Setup
1. Clone the repo
  * git clone https://github.com/jonathanpaulson/minions.git
2. Install Java/Scala/sbt. We'll use https://sdkman.io/
  * $ curl -s "https://get.sdkman.io" | bash
  * $ sdk install java 8.0.242.j9-adpt
  * $ sdk install scala 2.13.1
  * $ sdk install sbt 1.3.9
3. Compile + Run
  * $ cd minions (go to directory where you cloned the repo)
  * $ sbt
  * $ buildEverything (in sbt console)
  * $ minionsJVM/run
4. Visit localhost:8080. Your local copy should be running there.

## AWS Setup
1. Reserve a medium instance (in AWS console)
2. Allocate an elastic IP (default settings; in AWS console)
3. Associate the elastic IP with the instance (in AWS console)

4. Setup SSH forwarding
  * $ eval `ssh-agent -s`
  * $ ssh-add ~/.ssh/id_rsa (on your box)
  * Edit ~/.ssh/config (on your box) to contain:
```
Host ec2-54-152-88-227.compute-1.amazonaws.com
  ForwardAgent yes
```

5. SSH to the instance
6. sudo yum install git -y (on instance)
7. git clone git@github.com:jonathanpaulson/minions.git (on instance)

8. Install Java 8 (locally on instance)
  1. wget --no-check-certificate --no-cookies --header "Cookie: oraclelicense=accept-securebackup-cookie" http://download.oracle.com/otn-pub/java/jdk/8u141-b15/336fa29ff2bb4ef291e347e091f7f4a7/jdk-8u141-linux-x64.rpm
  * sudo yum install -y jdk-8u141-linux-x64.rpm

9. Install SBT (locally on instance).
  * curl https://bintray.com/sbt/rpm/rpm | sudo tee /etc/yum.repos.d/bintray-sbt-rpm.repo
  * sudo yum install sbt

10. Edit AWS security groups to allow port 80 (in AWS console).

11. Edit application.conf file in minions (locally on instance).
  * Change "interface" to your instance's *private* IP
  * Change port to 80

12. Build + run server (locally on instance). Do this in a tmux session so it stays up.
  * $ cd minions
  * $ sudo sbt (need sudo to connect to port 80)
  * $ buildEverything (in sbt console)
  * $ minionsJVM/run (in sbt console)

13. It's running. Open a web browser at your instance's public IP. Anyone can use this.

## Contributors

* Jonathan Paulson
* David Wu
