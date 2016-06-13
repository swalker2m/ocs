package edu.gemini.spModel.io.impl.migration

import edu.gemini.pot.sp.SPComponentType
import edu.gemini.spModel.pio.{Container, ContainerParent, ParamSet}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.Try

object PioSyntax {

  implicit class ContainerParentOps(p: ContainerParent) {

    def containers: List[Container] =
      p.getContainers.asInstanceOf[java.util.List[Container]].asScala.toList

    def allContainers: List[Container] = {
      val cs = containers
      cs ++ cs.flatMap(_.allContainers)
    }

    def findContainers(spc: SPComponentType): List[Container] =
      allContainers.filter(_.componentType.contains(spc))

  }

  implicit class ContainerOps(c: Container) {

    def componentType: Option[SPComponentType] =
      Try(SPComponentType.getInstance(c.getType, c.getSubtype)).toOption

    def paramSets: List[ParamSet] =
      c.getParamSets.asInstanceOf[java.util.List[ParamSet]].asScala.toList

    def allParamSets: List[ParamSet] = {
      val ps = paramSets
      ps ++ ps.flatMap(_.allParamSets)
    }

  }

  implicit final class ParamSetOps(p: ParamSet) {

    def paramSets: List[ParamSet] =
      p.getParamSets.asScala.toList

    def allParamSets: List[ParamSet] = {
      val ps = paramSets
      ps ++ ps.flatMap(_.allParamSets)
    }

    def value(key: String): Option[String] =
      Option(p.getParam(key)).map(_.getValue)

    def double(key: String): Option[Double] =
      Option(p.getParam(key)).map(_.getValue.toDouble)

    @tailrec
    def removeChildren(name: String): Unit =
      if (p.removeChild(name) == null) () else removeChildren(name)
  }

}
