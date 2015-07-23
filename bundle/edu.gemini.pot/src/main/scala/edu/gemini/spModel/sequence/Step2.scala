package edu.gemini.spModel.sequence

import scalaz._, Scalaz._

sealed trait Step2[I] {
  def instrument: I
}

final case class ScienceStep[I](instrument: I, telescope: Telescope) extends Step2[I]

object ScienceStep {
  implicit def describeScienceStep[I: Describe]: Describe[ScienceStep[I]] =
    Describe[(I, Telescope)].xmap[ScienceStep[I]](
      t => ScienceStep(t._1, t._2),
      s => (s.instrument, s.telescope)
    )
}

final case class DarkStep[I](instrument: I) extends Step2[I]

object DarkStep {
  implicit def describeDarkStep[I: Describe]: Describe[DarkStep[I]] =
    Describe[I].xmap[DarkStep[I]](
      i => DarkStep(i),
      s => s.instrument
    )

  val ds = DarkStep[F2](F2(F2.Filter.H, F2.Disperser.None))
}

final case class BiasStep[I](instrument: I) extends Step2[I]

object BiasStep {
  implicit def describeBiasStep[I: Describe]: Describe[BiasStep[I]] =
    Describe[I].xmap[BiasStep[I]](
      i => BiasStep(i),
      s => s.instrument
    )
}

final case class GcalStep[I](instrument: I, gcal: Gcal) extends Step2[I]

object GcalStep {
  implicit def describeGcalStep[I: Describe]: Describe[GcalStep[I]] =
    Describe[(I, Gcal)].xmap[GcalStep[I]](
      t => GcalStep(t._1, t._2),
      s => (s.instrument, s.gcal)
    )
}

final case class SmartStep[I](instrument: I, smartStepType: SmartStep.Type) extends Step2[I]

object SmartStep {
  sealed trait Type
  object Type {
    case object Arc           extends Type
    case object Flat          extends Type
    case object DayBaseline   extends Type
    case object NightBaseline extends Type
  }

  implicit def describeSmartStep[I: Describe]: Describe[SmartStep[I]] = {
    object TypeProp extends Prop[SmartStep[I]] {
      type B = Type
      val eq: Equal[Type] = Equal.equalA

      def lens: SmartStep[I] @> Type = Lens.lensu((a, b) => a.copy(smartStepType = b), _.smartStepType)
    }

    val inst: SmartStep[I] @> I = Lens.lensu((a, b) => a.copy(instrument = b), _.instrument)

    new Describe[SmartStep[I]] {
      val props: List[Prop[SmartStep[I]]] =
        TypeProp :: implicitly[Describe[I]].props.map(_ compose inst)
    }
  }
}