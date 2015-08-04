package edu.gemini.spModel.sequence

import scalaz._, Scalaz._

sealed trait Step2[I] {
  def instrument: I
  def stepType: Step2.Type
}

object Step2 {

  // Step type is used in deserializing a sequence.  Given a specific step
  // type we can use the corresponding Default type class instance to create a
  // default value for that step type and then apply changes to it.
  sealed trait Type
  case object Bias    extends Type
  case object Dark    extends Type
  case object Gcal    extends Type
  case object Science extends Type
  case object Smart   extends Type

  implicit def EqualType: Equal[Type] = Equal.equalA
}

final case class BiasStep[I](instrument: I) extends Step2[I] {
  def stepType = Step2.Bias
}

object BiasStep {
  implicit def describeBiasStep[I: Describe]: Describe[BiasStep[I]] =
    Describe[I].xmap[BiasStep[I]](
      i => BiasStep(i),
      s => s.instrument
    )
}

final case class DarkStep[I](instrument: I) extends Step2[I] {
  def stepType = Step2.Dark
}

object DarkStep {
  implicit def describeDarkStep[I: Describe]: Describe[DarkStep[I]] =
    Describe[I].xmap[DarkStep[I]](
      i => DarkStep(i),
      s => s.instrument
    )
}

final case class GcalStep[I](instrument: I, gcal: GcalUnit) extends Step2[I] {
  def stepType = Step2.Gcal
}

object GcalStep {
  implicit def describeGcalStep[I: Describe]: Describe[GcalStep[I]] =
    Describe[(I, GcalUnit)].xmap[GcalStep[I]](
      t => GcalStep(t._1, t._2),
      s => (s.instrument, s.gcal)
    )
}

final case class ScienceStep[I](instrument: I, telescope: Telescope) extends Step2[I] {
  def stepType = Step2.Science
}

object ScienceStep {
  implicit def describeScienceStep[I: Describe]: Describe[ScienceStep[I]] =
    Describe[(I, Telescope)].xmap[ScienceStep[I]](
      t => ScienceStep(t._1, t._2),
      s => (s.instrument, s.telescope)
    )
}

final case class SmartStep[I](instrument: I, smartStepType: SmartStep.Type) extends Step2[I] {
  def stepType = Step2.Smart
}

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
      val default: SmartStep[I] =
        SmartStep(implicitly[Describe[I]].default, SmartStep.Type.Arc)

      val props: List[Prop[SmartStep[I]]] =
        TypeProp :: implicitly[Describe[I]].props.map(_ compose inst)
    }
  }
}
