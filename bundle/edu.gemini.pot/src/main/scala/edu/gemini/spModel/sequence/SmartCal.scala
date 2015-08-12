package edu.gemini.spModel.sequence

import edu.gemini.spModel.sequence.Metadata.Access.Science
import edu.gemini.spModel.sequence.Metadata.Scope.SingleStep
import edu.gemini.spModel.sequence.Metadata.{Attrs, Label}

import scalaz._

final case class SmartCal(smartCalType: SmartCal.Type)

object SmartCal {
  sealed trait Type
  object Type {
    case object Arc           extends Type
    case object Flat          extends Type
    case object DayBaseline   extends Type
    case object NightBaseline extends Type

    val Default = Arc
    val All     = NonEmptyList(Arc, Flat, DayBaseline, NightBaseline)
  }

  val Lab = Label("Smart")

  object TypeProp extends Prop[SmartCal] {
    type B = Type
    val eq: Equal[Type] = Equal.equalA

    def lens: SmartCal @> Type = Lens.lensu((a, b) => a.copy(smartCalType = b), _.smartCalType)

    val meta = EnumMetadata[Type](Attrs(Label(Lab, "Cal Type"), Science, SingleStep), Type.All)
  }

  implicit val DescribeSmartCal: Describe[SmartCal] =
    Describe.forProps(
      SmartCal(SmartCal.Type.Default),
      TypeProp
    )
}
