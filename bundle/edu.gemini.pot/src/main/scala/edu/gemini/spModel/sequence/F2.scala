package edu.gemini.spModel.sequence

import scalaz._
import Scalaz._

final case class F2(filter: F2.Filter, disperser: F2.Disperser)

object F2 {
//  import Metadata.Scope._
//  import Metadata.Access._

  sealed trait Filter
  object Filter {
    case object J extends Filter
    case object H extends Filter
    case object K extends Filter
  }
//    val attrs = Metadata.Attributes("Filter", SingleStep, Science)
//    val desc  = Metadata.EnumDescriptor(attrs, NonEmptyList(J, H, K))

  object FilterProp extends Prop[F2] {
    type B = Filter
    val eq: Equal[Filter]  = Equal.equalA
    val lens: F2 @> Filter = Lens.lensu((a,b) => a.copy(filter = b), _.filter)
  }

  sealed trait Disperser
  object Disperser {
    case object None extends Disperser
    case object JH   extends Disperser
    case object HK   extends Disperser
  }

  object DisperserProp extends Prop[F2] {
    type B = Disperser
    val eq: Equal[Disperser]  = Equal.equalA
    val lens: F2 @> Disperser = Lens.lensu((a,b) => a.copy(disperser = b), _.disperser)
  }

  implicit val DescribeF2: Describe[F2] =
    Describe.forProps(FilterProp, DisperserProp)

  implicit val DefaultF2: Default[F2] =
    Default.forValue(F2(F2.Filter.J, F2.Disperser.None))
}