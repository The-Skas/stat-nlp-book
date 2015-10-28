name := "statnlp"



version := "0.1"

scalaVersion := "2.11.4"


libraryDependencies ++= Seq(
  "com.quantifind" %% "wisp" % "0.0.1" excludeAll ExclusionRule(organization = "javax.servlet")
)

