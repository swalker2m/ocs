import OcsKeys._

// note: inter-project dependencies are declared at the top, in projects.sbt

name := "jsky.app.ot.visitlog"

// version set in ThisBuild

unmanagedJars in Compile ++= Seq(
  new File(baseDirectory.value, "../../lib/bundle/org-jdesktop-swingx_2.10-1.6.4.jar"))

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % ScalaZVersion)

osgiSettings

ocsBundleSettings

OsgiKeys.bundleActivator := Some("jsky.app.ot.visitlog.osgi.Activator")

OsgiKeys.bundleSymbolicName := name.value

OsgiKeys.dynamicImportPackage := Seq("")

OsgiKeys.exportPackage := Seq(
  )
