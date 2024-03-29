lazy val root = (project in file("."))
  .settings(
    name := "AdventOfCode",
  )
  .aggregate(base, aoc21)

lazy val base = project
lazy val aoc21 = project.dependsOn(base % "compile->compile; test->test")

ThisBuild / version := "0.1.0-SNAPSHOT"

// Scala 2.x version https://www.scala-lang.org/download/all.html
ThisBuild / scalaVersion := "2.13.11"

// https://www.scalatest.org/install
ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.16" % "test"

// https://www.scalatest.org/plus/scalacheck
ThisBuild / libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % "test"

// https://scalacheck.org/download.html, do not trust the version shown there but check from "The Central Repository".
ThisBuild / libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % "test"

// https://typelevel.org/cats/
ThisBuild / libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
//libraryDependencies += "org.typelevel" %% "cats-collections-core" % "7.3.7"

// https://docs.scala-lang.org/overviews/compiler-options/index.html
ThisBuild / scalacOptions ++= Seq(
  // Standard Settings
  "-encoding", "utf-8", // Specify character encoding used by source files.
  "-deprecation", // Emit warning and location for usages of deprecated APIs.
  "-explaintypes", // Explain type errors in more detail.
  "-feature", // Emit warning and location for usages of features that should be imported explicitly.
  "-language:higherKinds", // Allow higher-kinded types
  "-unchecked", // Enable additional warnings where generated code depends on assumptions.
  //  "-Werror", // Fail the compilation if there are any warnings.
  //
  // Advanced Settings
  //"-Xdev", // Indicates user is a developer - issue warnings about anything which seems amiss // Not used, as
  // https://gitter.im/scala/scala/archives/2019/08/22?at=5d5e62f95ba4a84f696d88c3
  // indicates that the actual use is
  // "Issue warnings about anything which seems amiss in compiler internals. Intended for compiler developers"
  //"-Xlint", // Enable all "lint" warnings. // Not used as I do not want to use "xlint:unused", see below.
  "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
  "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
  "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
  "-Xlint:infer-any", // Warn when a type argument is inferred to be Any.
  "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
  "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
  "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
  "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
  "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
  "-Xlint:option-implicit", // Option.apply used implicit view.
  "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
  "-Xlint:package-object-classes", // Class or object defined in package object.
  "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
  "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
  //"-Xlint:unused", // Enable -Wunused:imports,privates,locals,implicits. // Not used, as I do not want to enable
  // "-Wunused:privates", see below.
  "-Xlint:nonlocal-return", // A return statement used an exception for flow control.
  "-Xlint:implicit-not-found", // Check @implicitNotFound and @implicitAmbiguous messages.
  "-Xlint:serial", // @SerialVersionUID on traits and non-serializable classes.
  "-Xlint:valpattern", // Enable pattern checks in val definitions.
  "-Xlint:eta-zero", // Warn on eta-expansion (rather than auto-application) of zero-ary method.
  "-Xlint:eta-sam", // Warn on eta-expansion to meet a Java-defined functional interface that is not explicitly
  // annotated with @FunctionalInterface.
  "-Xlint:deprecation", // Enable linted deprecations.
  //
  // Warning Settings
  "-Wdead-code", // Warn when dead code is identified.
  "-Wextra-implicit", // Warn when more than one implicit parameter section is defined.
  "-Wnumeric-widen", // Warn when numerics are widened.
  "-Woctal-literal", // Warn on obsolete octal syntax.
  //"-Wunused", // Enables all "unused" warnings. // Not used, as I do not want to enable "Wunused:privates", see below.
  //"-Wunused:imports", // Warn if an import selector is not referenced.
  "-Wunused:patvars", // Warn if a variable bound in a pattern is unused.
  //"-Wunused:privates", // Warn if a private member is unused. // Not used, as I often need to make the copy method
  // private, so that illegal state cannot be constructed through the copy method.
  // The IDE will issue a warning which I can squelch via "// noinspection ScalaUnusedSymbol".
  //"-Wunused:locals", // Warn if a local definition is unused.
  "-Wunused:explicits", // Warn if an explicit parameter is unused.
  "-Wunused:implicits", // Warn if an implicit parameter is unused.
  "-Wunused:params", //Enable -Wunused:explicits,implicits.
  "-Wunused:linted", // The same as "-Xlint:unused", which is the same as
  "-Wunused:imports,privates,locals,implicits", // Not used, as I do not want to use "-Wunused:privates", see above.
  "-Wvalue-discard", // Warn when non-Unit expression results are unused.
)
