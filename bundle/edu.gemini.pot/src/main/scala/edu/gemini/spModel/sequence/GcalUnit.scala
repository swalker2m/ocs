package edu.gemini.spModel.sequence

import edu.gemini.spModel.gemini.calunit.CalUnitParams.{Lamp, Shutter}
import edu.gemini.spModel.sequence.Metadata.Access.Science
import edu.gemini.spModel.sequence.Metadata.Scope.SingleStep

import scalaz._, Scalaz._

case class GcalUnit(lamp: Lamp, shutter: Shutter)

object GcalUnit {
//  sealed trait Lamp
//  object Lamp {
//    case object IrHigh extends Lamp
//    case object IrLow  extends Lamp
//    case object Quartz extends Lamp
//
//    val All = NonEmptyList(IrHigh, IrLow, Quartz)
//  }

  object LampProp extends Prop[GcalUnit] {
    type B = Lamp
    val eq: Equal[Lamp]    = Equal.equalA
    val lens: GcalUnit @> Lamp = Lens.lensu((a,b) => a.copy(lamp = b), _.lamp)

    val meta = EnumMetadata[Lamp](
      "lamp",
      Science,
      SingleStep,
      Lamp.values(),
      _.toString)
  }

//  sealed trait Shutter
//  object Shutter {
//    case object Open   extends Shutter
//    case object Closed extends Shutter
//
//    val All = NonEmptyList(Open, Closed)
//  }

  object ShutterProp extends Prop[GcalUnit] {
    type B = Shutter
    val eq: Equal[Shutter]    = Equal.equalA
    val lens: GcalUnit @> Shutter = Lens.lensu((a,b) => a.copy(shutter = b), _.shutter)

    val meta = EnumMetadata[Shutter]("shutter",
      Science,
      SingleStep,
      Shutter.values(),
      _.toString)
  }

  implicit val DescribeGcal: Describe[GcalUnit] =
    Describe.forProps(
      GcalUnit(Lamp.DEFAULT, Shutter.DEFAULT),
      LampProp, ShutterProp
    )
}