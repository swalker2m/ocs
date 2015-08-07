package edu.gemini.spModel.sequence

import edu.gemini.spModel.gemini.calunit.CalUnitParams.{Lamp, Shutter}
import edu.gemini.spModel.sequence.Metadata.Access.Science
import edu.gemini.spModel.sequence.Metadata.Scope.SingleStep

import scalaz._, Scalaz._

case class GcalUnit(lamp: Lamp, shutter: Shutter)

object GcalUnit {
  import EnumMetadata.fromJava

  object LampProp extends Prop[GcalUnit] {
    type B = Lamp
    val eq: Equal[Lamp]    = Equal.equalA
    val lens: GcalUnit @> Lamp = Lens.lensu((a,b) => a.copy(lamp = b), _.lamp)

    val meta = fromJava("lamp", Science, SingleStep, classOf[Lamp])
  }


  object ShutterProp extends Prop[GcalUnit] {
    type B = Shutter
    val eq: Equal[Shutter]    = Equal.equalA
    val lens: GcalUnit @> Shutter = Lens.lensu((a,b) => a.copy(shutter = b), _.shutter)

    val meta = fromJava("shutter", Science, SingleStep, classOf[Shutter])
  }

  implicit val DescribeGcal: Describe[GcalUnit] =
    Describe.forProps(
      GcalUnit(Lamp.DEFAULT, Shutter.DEFAULT),
      LampProp, ShutterProp
    )
}