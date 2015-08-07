package edu.gemini.spModel.sequence

import edu.gemini.pot.sp.SPComponentType
import edu.gemini.spModel.gemini.flamingos2.Flamingos2.{Filter, Disperser}

import scalaz._
import Scalaz._

final case class F2(filter: Filter, disperser: Disperser) extends Instrument {
  def name = "F2"
  def componentType = SPComponentType.INSTRUMENT_FLAMINGOS2
}

object F2 {
  import Metadata.Scope._
  import Metadata.Access._

//  sealed abstract class Filter(val name: String, val log: String, val wavelength: Option[Double]) extends Serializable
//  object Filter {
//    case object J     extends Filter("J (1.25 um)",      "J",      Some(1.25))
//    case object H     extends Filter("H (1.65 um)",      "H",      Some(1.65))
//    case object KLong extends Filter("K-long (2.00 um)", "K-long", Some(2.00))
//
//    val All = NonEmptyList(J, H, KLong)
//  }

  object FilterProp extends Prop[F2] {
    type B = Filter
    val eq: Equal[Filter]  = Equal.equalA
    val lens: F2 @> Filter = Lens.lensu((a,b) => a.copy(filter = b), _.filter)

    val meta  = EnumMetadata[Filter](
      "filter",
      Science,
      SingleStep,
      Filter.values(),
      _.logValue)
  }

//  sealed abstract class Disperser(val name: String, val log: String, val wavelength: Option[Double]) extends Serializable
//  object Disperser {
//    case object None    extends Disperser("None",                       "none",    none)
//    case object R1200JH extends Disperser("R=1200 (J + H) grism",       "R1200JH", some(1.390))
//    case object R1200HK extends Disperser("R=1200 (H + K) grism",       "R1200HK", some(1.871))
//    case object R3000   extends Disperser("R=3000 (J or H or K) grism", "R3000",   some(1.650))
//
//    val All = NonEmptyList(None, R1200JH, R1200HK, R3000)
//  }

  object DisperserProp extends Prop[F2] {
    type B = Disperser
    val eq: Equal[Disperser]  = Equal.equalA
    val lens: F2 @> Disperser = Lens.lensu((a,b) => a.copy(disperser = b), _.disperser)

    val meta = EnumMetadata[Disperser](
      "disperser",
      Science,
      SingleStep,
      Disperser.values(),
      _.logValue)
  }

  implicit val DescribeF2: Describe[F2] =
    Describe.forProps(
      F2(Filter.DEFAULT, Disperser.DEFAULT),
      FilterProp, DisperserProp
    )

}