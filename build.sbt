lazy val root = project
  .in( file( "." ) )
  .settings(
    name         := "BrainFuck",
    organization := "br.com.hmsoftware",
    version      := "1.0",
    scalaVersion := "2.12.8",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.0.5" % "test" ) )

