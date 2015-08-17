package edu.gemini.spModel.sequence

import scalaz._, Scalaz._

sealed trait Step[I] {
  def instrument: I
  def stepType: Step.Type
}

object Step {

  // Step type is used in deserializing a sequence.  Given a specific step
  // type we can use the corresponding Default type class instance to create a
  // default value for that step type and then apply changes to it.
  sealed trait Type
  case object Bias    extends Type
  case object Dark    extends Type
  case object Gcal    extends Type
  case object Science extends Type
  case object Smart   extends Type

  implicit val EqualType: Equal[Type] = Equal.equalA

  implicit def showStep[I : Describe]: Show[Step[I]] = {
    def showVals[A : Describe](name: String, a: A): String = {
      val props  = implicitly[Describe[A]].props
      val values = props.map { p =>
        val b = p.lens.get(a)
        s"${p.meta.attrs.label.name} = ${p.meta.show(b)}"
      }

      values.mkString(name + " {", ", ", "}")
    }

    Show.shows[Step[I]] {
      case BiasStep(i)       => "Bias: "      + showVals("inst", i)
      case DarkStep(i)       => "Dark: "      + showVals("inst", i)
      case GcalStep(i, g)    => "Gcal: "      + showVals("inst", i) + " " + showVals("gcal", g)
      case ScienceStep(i, t) => "Science: "   + showVals("inst", i) + " " + showVals("telescope", t)
      case SmartStep(i, t)   => s"Smart $t: " + showVals("inst", i)
    }
  }

  def instrument[I]: Step[I] @?> I = PLens.plensgf({
      case BiasStep(_)               => (i: I) => BiasStep(i)
      case DarkStep(_)               => (i: I) => DarkStep(i)
      case GcalStep(_, gcal)         => (i: I) => GcalStep(i, gcal)
      case ScienceStep(_, telescope) => (i: I) => ScienceStep(i, telescope)
      case SmartStep(_, smart)       => (i: I) => SmartStep(i, smart)
    }, {
      case BiasStep(i)       => i
      case DarkStep(i)       => i
      case GcalStep(i, _)    => i
      case ScienceStep(i, _) => i
      case SmartStep(i, _)   => i
    })

  def gcal[I]: Step[I] @?> GcalUnit = PLens.plensgf({
      case GcalStep(inst, _) => (g: GcalUnit) => GcalStep(inst, g)
    }, { case GcalStep(_, g) => g })

  def telescope[I]: Step[I] @?> Telescope = PLens.plensgf({
      case ScienceStep(inst,_) => (t: Telescope) => ScienceStep(inst, t)
    }, { case ScienceStep(_, t) => t })

  def smartCal[I]: Step[I] @?> SmartCal = PLens.plensgf({
      case SmartStep(inst,_) => (s: SmartCal) => SmartStep(inst, s)
    }, { case SmartStep(_, s) => s})
}

final case class BiasStep[I](instrument: I) extends Step[I] {
  def stepType = Step.Bias
}

object BiasStep {
  implicit def describeBiasStep[I: Describe]: Describe[BiasStep[I]] =
    Describe[I].xmap[BiasStep[I]](
      i => BiasStep(i),
      s => s.instrument
    )
}

final case class DarkStep[I](instrument: I) extends Step[I] {
  def stepType = Step.Dark
}

object DarkStep {
  implicit def describeDarkStep[I: Describe]: Describe[DarkStep[I]] =
    Describe[I].xmap[DarkStep[I]](
      i => DarkStep(i),
      s => s.instrument
    )
}

final case class GcalStep[I](instrument: I, gcal: GcalUnit) extends Step[I] {
  def stepType = Step.Gcal
}

object GcalStep {
  implicit def describeGcalStep[I: Describe]: Describe[GcalStep[I]] =
    Describe[(I, GcalUnit)].xmap[GcalStep[I]](
      t => GcalStep(t._1, t._2),
      s => (s.instrument, s.gcal)
    )
}

final case class ScienceStep[I](instrument: I, telescope: Telescope) extends Step[I] {
  def stepType = Step.Science
}

object ScienceStep {
  implicit def describeScienceStep[I: Describe]: Describe[ScienceStep[I]] =
    Describe[(I, Telescope)].xmap[ScienceStep[I]](
      t => ScienceStep(t._1, t._2),
      s => (s.instrument, s.telescope)
    )
}

final case class SmartStep[I](instrument: I, smartCal: SmartCal) extends Step[I] {
  def stepType = Step.Smart
}

object SmartStep {
  implicit def describeSmartStep[I: Describe]: Describe[SmartStep[I]] =
    Describe[(I, SmartCal)].xmap[SmartStep[I]](
      t => SmartStep(t._1, t._2),
      s => (s.instrument, s.smartCal)
    )
}
