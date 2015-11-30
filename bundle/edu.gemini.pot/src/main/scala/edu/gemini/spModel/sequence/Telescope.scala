package edu.gemini.spModel.sequence

import edu.gemini.spModel.core.AngleSyntax._
import edu.gemini.spModel.core.{OffsetQ, OffsetP, Offset}
import edu.gemini.spModel.sequence.Metadata.Access.Science
import edu.gemini.spModel.sequence.Metadata.{Attrs, Label}
import edu.gemini.spModel.sequence.Metadata.Scope.SingleStep

import scalaz._
import Scalaz._

final case class Telescope(p: OffsetP, q: OffsetQ) {
  def offset: Offset = Offset(p, q)
}

object Telescope {
  val Lab = Label("Telescope")

  def lab(name: String): Label = Label(Lab, name)

  object OffsetPProp extends Prop[Telescope] {
    type B = OffsetP
    val eq: Equal[OffsetP]         = Equal[OffsetP]
    val lens: Telescope @> OffsetP = Lens.lensu((a, b) => a.copy(p = b), _.p)

    val meta = new TextMetadata[OffsetP](
      Attrs(lab("p"), Science, SingleStep),
      Some("arcsec"),
      p => f"${p.arcsecs}%4.03f",
      doubleParser("Offset p", _)(_.arcsecs[OffsetP])
    )
  }

  object OffsetQProp extends Prop[Telescope] {
    type B = OffsetQ
    val eq: Equal[OffsetQ]         = Equal[OffsetQ]
    val lens: Telescope @> OffsetQ = Lens.lensu((a, b) => a.copy(q = b), _.q)

    val meta = new TextMetadata[OffsetQ](
      Attrs(lab("q"), Science, SingleStep),
      Some("arcsec"),
      q => f"${q.arcsecs}%4.03f",
      doubleParser("Offset q", _)(_.arcsecs[OffsetQ])
    )
  }

  implicit val DescribeTelescope: Describe[Telescope] =
    Describe.forProps(
      Telescope(OffsetP.Zero, OffsetQ.Zero),
      OffsetPProp, OffsetQProp
    )
}