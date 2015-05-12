name := "relation_extraction"

version := "1.0"

scalaVersion := "2.11.6"


resolvers ++= Seq(
  //    "Local Maven Repo" at "file://" + Path.userHome.absolutePath + "/.m2/repository"
  //  "Local Maven Repo" at "file://" + Path.userHome.absolutePath +"/FNLP" ,
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)


libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-mllib" % "1.3.1",
  "org.scalaj" %% "scalaj-http" % "1.1.4",
  "org.scala-lang.modules" %% "scala-pickling" % "0.10.0",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.1",
  //  "edu.stanford.nlp" % "stanford-corenlp" % "3.5.1" classifier "models",
  "org.scalanlp" %% "breeze" % "0.11.2",
  "com.typesafe.play" %% "play-json" % "2.4.0-M3" ,
"com.github.nscala-time" %% "nscala-time" % "2.0.0"
)

//unmanagedClasspath += baseDirectory.value / ""
