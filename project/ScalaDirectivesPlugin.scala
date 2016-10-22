package akka

import sbt._

object DocsGeneratorPlugin extends AutoPlugin {
  override def requires = plugins.JvmPlugin
  override def trigger = AllRequirements

  override def extraProjects = Seq(
    Project(base = file("project/docs-generator"), id = "docs-generator")
  )
}
