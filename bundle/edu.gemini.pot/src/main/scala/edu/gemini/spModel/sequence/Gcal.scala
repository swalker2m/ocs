package edu.gemini.spModel.sequence

import scalaz._, Scalaz._

case class Gcal(lamp: Gcal.Lamp, shutter: Gcal.Shutter)

object Gcal {
  sealed trait Lamp
  object Lamp {
    case object IrHigh extends Lamp
    case object IrLow  extends Lamp
    case object Quartz extends Lamp
  }

  object LampProp extends Prop[Gcal] {
    type B = Lamp
    val eq: Equal[Lamp]    = Equal.equalA
    val lens: Gcal @> Lamp = Lens.lensu((a,b) => a.copy(lamp = b), _.lamp)
  }

  sealed trait Shutter
  object Shutter {
    case object Open   extends Shutter
    case object Closed extends Shutter
  }

  object ShutterProp extends Prop[Gcal] {
    type B = Shutter
    val eq: Equal[Shutter]    = Equal.equalA
    val lens: Gcal @> Shutter = Lens.lensu((a,b) => a.copy(shutter = b), _.shutter)
  }

  implicit val DescribeGcal: Describe[Gcal] =
    Describe.forProps(LampProp, ShutterProp)
}