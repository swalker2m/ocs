package edu.gemini.spModel.sequence

import scalaz._, Scalaz._

case class GcalUnit(lamp: GcalUnit.Lamp, shutter: GcalUnit.Shutter)

object GcalUnit {
  sealed trait Lamp
  object Lamp {
    case object IrHigh extends Lamp
    case object IrLow  extends Lamp
    case object Quartz extends Lamp
  }

  object LampProp extends Prop[GcalUnit] {
    type B = Lamp
    val eq: Equal[Lamp]    = Equal.equalA
    val lens: GcalUnit @> Lamp = Lens.lensu((a,b) => a.copy(lamp = b), _.lamp)
  }

  sealed trait Shutter
  object Shutter {
    case object Open   extends Shutter
    case object Closed extends Shutter
  }

  object ShutterProp extends Prop[GcalUnit] {
    type B = Shutter
    val eq: Equal[Shutter]    = Equal.equalA
    val lens: GcalUnit @> Shutter = Lens.lensu((a,b) => a.copy(shutter = b), _.shutter)
  }

  implicit val DescribeGcal: Describe[GcalUnit] =
    Describe.forProps(LampProp, ShutterProp)

  implicit val DefaultGcal: Default[GcalUnit] =
    Default.forValue(GcalUnit(GcalUnit.Lamp.IrHigh, GcalUnit.Shutter.Open))
}