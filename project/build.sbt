unmanagedSourceDirectories in Compile ++= {
  val root = baseDirectory.value.getParentFile

  (root / "sbt-scala-native/src/main/scala-sbt-0.13") +:
    Seq(
    "util",
    "nir",
    "tools",
    "sbt-scala-native",
    "test-interface-serialization",
    "test-runner"
  ).map(dir => root / s"$dir/src/main/scala")
}

libraryDependencies ++= Seq(
  "org.scala-sbt"      % "scripted-plugin"      % sbtVersion.value,
  "org.eclipse.jgit"   % "org.eclipse.jgit.pgm" % "3.7.1.201504261725-r",
  "org.zeroturnaround" % "zt-zip"               % "1.13"
)

addSbtPlugin("org.portable-scala" % "sbt-platform-deps" % "1.0.0")
addSbtPlugin("com.eed3si9n"       % "sbt-dirty-money"   % "0.2.0")
addSbtPlugin("org.foundweekends"  % "sbt-bintray"       % "0.5.4")
addSbtPlugin("com.jsuereth"       % "sbt-pgp"           % "1.0.0")
addSbtPlugin("com.typesafe"       % "sbt-mima-plugin"   % "0.3.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"           % "0.3.7")
addSbtPlugin("io.get-coursier"    % "sbt-coursier"      % "1.1.0-M7")

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-encoding",
  "utf8"
)
