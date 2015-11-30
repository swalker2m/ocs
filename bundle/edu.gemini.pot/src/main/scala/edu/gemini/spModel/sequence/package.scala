package edu.gemini.spModel

import scalaz._
import Scalaz._

package object sequence {
  val DoublePat = """([-+]?\d*\.?\d+)""".r

  def doubleParser[A](label: String, s: String)(f: Double => A): String \/ A =
    s match {
      case DoublePat(d) => f(d.toDouble).right
      case _            => s"$label value `$s` could not be parsed.".left
    }
}
