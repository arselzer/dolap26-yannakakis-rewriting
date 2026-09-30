ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.13"

lazy val munitVersion = "1.3.5"
lazy val munitScalaCheckVersion = "1.3.0"
lazy val testcontainersVersion = "2.0.5"

lazy val root = (project in file("."))
  .settings(
    name := "rewrite",
    Compile / mainClass := Some("at.ac.tuwien.dbai.yannakakis.cli.PostgreSqlRewriteCli"),
    assembly / mainClass := Some("at.ac.tuwien.dbai.yannakakis.cli.PostgreSqlRewriteCli"),
    libraryDependencies ++= Seq(
      "org.apache.calcite" % "calcite-core" % "1.36.0",
      "org.apache.calcite" % "calcite-csv" % "1.36.0",
      "com.fasterxml.jackson.core" % "jackson-databind" % "2.15.2",
      "org.postgresql" % "postgresql" % "42.7.2",
      "com.lihaoyi" %% "upickle" % "3.0.0-M2",
      "org.scalameta" %% "munit" % munitVersion % Test,
      "org.scalameta" %% "munit-scalacheck" % munitScalaCheckVersion % Test
    )
  )

lazy val integrationTests = (project in file("integration-tests"))
  .dependsOn(root)
  .settings(
    name := "rewrite-integration-tests",
    publish / skip := true,
    Test / parallelExecution := false,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % munitVersion % Test,
      "org.testcontainers" % "testcontainers-postgresql" % testcontainersVersion % Test
    )
  )

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}
