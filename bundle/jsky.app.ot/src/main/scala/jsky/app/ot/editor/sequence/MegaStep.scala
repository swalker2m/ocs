package jsky.app.ot.editor.sequence

import edu.gemini.spModel.sequence._

import scalaz._

final case class MegaStep[I](instrument: I, telescope: Telescope, gcal: GcalUnit, smartCal: SmartCal)

object MegaStep {
  implicit def describeMegaStep[I: Describe]: Describe[MegaStep[I]] =
    Describe[(I, Telescope, GcalUnit, SmartCal)].xmap[MegaStep[I]](
      t => MegaStep(t._1, t._2, t._3, t._4),
      s => (s.instrument, s.telescope, s.gcal, s.smartCal)
    )

  def availableProps[I: Describe](ss: List[Step[I]]): Set[Prop[MegaStep[I]]] = {

    def props[A: Describe](set: (MegaStep[I], A) => MegaStep[I], get: MegaStep[I] => A): List[Prop[MegaStep[I]]] = {
      val ln = Lens.lensu[MegaStep[I], A](set, get)
      implicitly[Describe[A]].props.map { _.compose(ln)}
    }

    def instProps: List[Prop[MegaStep[I]]] =
      props[I]((a,b) => a.copy(instrument = b), _.instrument)

    def gcalProps: List[Prop[MegaStep[I]]] =
      props[GcalUnit]((a,b) => a.copy(gcal = b), _.gcal)

    def telescopeProps: List[Prop[MegaStep[I]]] =
      props[Telescope]((a,b) => a.copy(telescope = b), _.telescope)

    def smartProps: List[Prop[MegaStep[I]]] =
      props[SmartCal]((a,b) => a.copy(smartCal = b), _.smartCal)

    (Set.empty[Prop[MegaStep[I]]]/:ss) { (set,step) =>
      val set0 = set ++ instProps
      step match {
        case BiasStep(_)       => set0
        case DarkStep(_)       => set0
        case GcalStep(_, _)    => set0 ++ gcalProps
        case ScienceStep(_, _) => set0 ++ telescopeProps
        case SmartStep(_, _)   => set0 ++ smartProps
      }
    }

  }
}
