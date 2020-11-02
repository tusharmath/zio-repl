scalaVersion := "2.13.3"

val zioVersion = "1.0.1+39-f8f72fc7-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "dev.zio" %% "zio"         % zioVersion
libraryDependencies += "dev.zio" %% "zio-streams" % "1.0.1"
