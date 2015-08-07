package edu.gemini.spModel.sequence

import edu.gemini.spModel.core.AngleSyntax._
import edu.gemini.spModel.core.{OffsetQ, OffsetP, Offset}
import edu.gemini.spModel.sequence.Metadata.Access.Science
import edu.gemini.spModel.sequence.Metadata.Attrs
import edu.gemini.spModel.sequence.Metadata.Scope.SingleStep

import scalaz._
import Scalaz._

final case class Telescope(p: OffsetP, q: OffsetQ) {
  def offset: Offset = Offset(p, q)
}

object Telescope {
  private val OffsetPat = """([-+]?\d*\.?\d+)""".r

  object OffsetPProp extends Prop[Telescope] {
    type B = OffsetP
    val eq: Equal[OffsetP]         = Equal[OffsetP]
    val lens: Telescope @> OffsetP = Lens.lensu((a, b) => a.copy(p = b), _.p)

    val meta = new TextMetadata[OffsetP](
      Attrs("p", "p", Science, SingleStep),
      Some("arcsec"),
      p => f"${p.arcsecs}%4.03f",
      {
        case OffsetPat(p) => p.toDouble.arcsecs[OffsetP].right
        case s            => s"Could not parse as offset in p: $s".left
      }
    )
  }

  object OffsetQProp extends Prop[Telescope] {
    type B = OffsetQ
    val eq: Equal[OffsetQ]         = Equal[OffsetQ]
    val lens: Telescope @> OffsetQ = Lens.lensu((a, b) => a.copy(q = b), _.q)

    val meta = new TextMetadata[OffsetQ](
      Attrs("q", "q", Science, SingleStep),
      Some("arcsec"),
      q => f"${q.arcsecs}%4.03f",
      {
        case OffsetPat(q) => q.toDouble.arcsecs[OffsetQ].right
        case s            => s"Could not parse as offset in q: $s".left
      }
    )
  }

  implicit val DescribeTelescope: Describe[Telescope] =
    Describe.forProps(
      Telescope(OffsetP.Zero, OffsetQ.Zero),
      OffsetPProp, OffsetQProp
    )
}