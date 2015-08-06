package edu.gemini.spModel.sequence

import edu.gemini.spModel.core.AngleSyntax.FromDegreesOps
import edu.gemini.spModel.core.{OffsetQ, OffsetP, Offset}
import edu.gemini.spModel.sequence.Metadata.Access.Science
import edu.gemini.spModel.sequence.Metadata.Scope.SingleStep

import scalaz._
import Scalaz._

final case class Telescope(p: OffsetP, q: OffsetQ) {
  def offset: Offset = Offset(p, q)
}

object Telescope {
  private val OffsetPat = """([-+]?\d*\.?\d+) arcsecs""".r

  object OffsetPProp extends Prop[Telescope] {
    type B = OffsetP
    val eq: Equal[OffsetP]         = Equal[OffsetP]
    val lens: Telescope @> OffsetP = Lens.lensu((a, b) => a.copy(p = b), _.p)

    val meta  = new Metadata[OffsetP] {
      override val name   = "p"
      override val access = Science
      override val scope  = SingleStep
      override val log    = (p: OffsetP) => p.shows

      override def serialize(a: OffsetP)  = a.shows
      override def deserialize(s: String) = s match {
        case OffsetPat(p) => p.toDouble.arcsecs[OffsetP].right
        case _            => s"Could not parse as offset in p: $s".left
      }
    }
  }

  object OffsetQProp extends Prop[Telescope] {
    type B = OffsetQ
    val eq: Equal[OffsetQ]         = Equal[OffsetQ]
    val lens: Telescope @> OffsetQ = Lens.lensu((a, b) => a.copy(q = b), _.q)

    val meta  = new Metadata[OffsetQ] {
      override val name   = "q"
      override val access = Science
      override val scope  = SingleStep
      override val log    = (q: OffsetQ) => q.shows

      override def serialize(a: OffsetQ)  = a.shows
      override def deserialize(s: String) = s match {
        case OffsetPat(q) => q.toDouble.arcsecs[OffsetQ].right
        case _            => s"Could not parse as offset in q: $s".left
      }
    }
  }

  implicit val DescribeTelescope: Describe[Telescope] =
    Describe.forProps(
      Telescope(OffsetP.Zero, OffsetQ.Zero),
      OffsetPProp, OffsetQProp
    )
}