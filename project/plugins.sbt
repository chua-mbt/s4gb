// Plugin versions. Library and Scala versions live in build.sbt.
val sbtScalajsVersion = "1.23.0"
val sbtScalajsCrossProjectVersion = "1.4.0"

addSbtPlugin("org.scala-js" % "sbt-scalajs" % sbtScalajsVersion)
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % sbtScalajsCrossProjectVersion)
