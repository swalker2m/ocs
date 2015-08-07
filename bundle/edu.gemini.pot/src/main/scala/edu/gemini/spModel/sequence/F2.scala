package edu.gemini.spModel.sequence

import edu.gemini.pot.sp.SPComponentType
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{Disperser, Filter, FPUnit, LyotWheel}

import Metadata.Scope._
import Metadata.Access._

import scalaz._
import Scalaz._

final case class F2(
    fpu: FPUnit,
    mosPreimaging: Boolean,
    filter: Filter,
    lyoutWheel: LyotWheel,
    disperser: Disperser
 ) extends Instrument {
  def name = "F2"
  def componentType = SPComponentType.INSTRUMENT_FLAMINGOS2
}

object F2 {

  import EnumMetadata.fromJava

  object FocalPlaneUnitProp extends Prop[F2] {
    type B = FPUnit
    val eq: Equal[FPUnit]  = Equal.equalA
    val lens: F2 @> FPUnit = Lens.lensu((a,b) => a.copy(fpu = b), _.fpu)

    val meta = fromJava("fpu", Science, SingleStep, classOf[FPUnit])
  }

  object MosPreimagingProp extends Prop[F2] {
    type B = Boolean
    val eq: Equal[Boolean]  = implicitly[Equal[Boolean]]
    val lens: F2 @> Boolean = Lens.lensu((a,b) => a.copy(mosPreimaging = b), _.mosPreimaging)

    val meta = BooleanMetadata("mosPreimaging", Science, Global)
  }

  object FilterProp extends Prop[F2] {
    type B = Filter
    val eq: Equal[Filter]  = Equal.equalA
    val lens: F2 @> Filter = Lens.lensu((a,b) => a.copy(filter = b), _.filter)

    val meta = fromJava("filter", Science, SingleStep, classOf[Filter])
  }

  object LyotWheelProp extends Prop[F2] {
    type B = LyotWheel
    val eq: Equal[LyotWheel]  = Equal.equalA
    val lens: F2 @> LyotWheel = Lens.lensu((a,b) => a.copy(lyoutWheel = b), _.lyoutWheel)

    val meta = fromJava("lyotWheel", Science, SingleStep, classOf[LyotWheel])
  }

  object DisperserProp extends Prop[F2] {
    type B = Disperser
    val eq: Equal[Disperser]  = Equal.equalA
    val lens: F2 @> Disperser = Lens.lensu((a,b) => a.copy(disperser = b), _.disperser)

    val meta = fromJava("disperser", Science, SingleStep, classOf[Disperser])
  }

  implicit val DescribeF2: Describe[F2] =
    Describe.forProps(
      F2(
        FPUnit.DEFAULT,
        mosPreimaging = false,
        Filter.DEFAULT,
        LyotWheel.DEFAULT,
        Disperser.DEFAULT
      ),
      FocalPlaneUnitProp,
      MosPreimagingProp,
      FilterProp,
      LyotWheelProp,
      DisperserProp
    )

}