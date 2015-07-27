package edu.gemini.spModel.sequence

import edu.gemini.spModel.core.{OffsetQ, OffsetP, Offset}

import scalaz._
import Scalaz._

final case class Telescope(p: OffsetP, q: OffsetQ) {
  def offset: Offset = Offset(p, q)
}

object Telescope {
  object OffsetPProp extends Prop[Telescope] {
    type B = OffsetP
    val eq: Equal[OffsetP]         = Equal[OffsetP]
    val lens: Telescope @> OffsetP = Lens.lensu((a, b) => a.copy(p = b), _.p)
  }

  object OffsetQProp extends Prop[Telescope] {
    type B = OffsetQ
    val eq: Equal[OffsetQ]         = Equal[OffsetQ]
    val lens: Telescope @> OffsetQ = Lens.lensu((a, b) => a.copy(q = b), _.q)
  }

  implicit val DescribeTelescope: Describe[Telescope] =
    Describe.forProps(OffsetPProp, OffsetQProp)

  implicit val DefaultTelescope: Default[Telescope] =
    Default.forValue(Telescope(OffsetP.Zero, OffsetQ.Zero))
}