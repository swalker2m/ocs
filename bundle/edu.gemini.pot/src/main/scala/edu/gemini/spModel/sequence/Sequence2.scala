package edu.gemini.spModel.sequence

import edu.gemini.spModel.sequence.Step2._

import scala.annotation.tailrec
import scala.language.existentials
import scala.reflect.ClassTag
import scalaz._
import Scalaz._

/** A sequence is an initial step and a list of diffs. */
final class Sequence2[I] private (private val ser: NonEmptyList[Sequence2.SerSteps]) extends Serializable {

  def toSteps(implicit ev: Describe[I]): NonEmptyList[Step2[I]] =
    ser.flatMap(_.toSteps)
}

object Sequence2 {

  // These type tags are used only in the deserialization process, so they are
  // made private and shouldn't escape the sequence implementation.
  private class SerSteps(stepType: Step2.Type, values: NonEmptyList[RunLength[_]]) extends Serializable {

    def toSteps[I: Describe]: NonEmptyList[Step2[I]] = {
      val rows = values.list.map(_.toList).transpose

      def steps[S <: Step2[I]](desc: Describe[S]): NonEmptyList[S] =
        rows.scanLeft(desc.default) { (s, r) =>
          (s/:r.zip(desc.props)) { case (s1, (a, p)) =>
            p.lens.set(s1, a.asInstanceOf[p.B])
          }
        }.tail.toNel.get

      stepType match {
        case Bias    => steps(implicitly[Describe[BiasStep[I]]])
        case Dark    => steps(implicitly[Describe[DarkStep[I]]])
        case Gcal    => steps(implicitly[Describe[GcalStep[I]]])
        case Science => steps(implicitly[Describe[ScienceStep[I]]])
        case Smart   => steps(implicitly[Describe[SmartStep[I]]])
      }
    }
  }

  def fromSteps[I: Describe](steps: NonEmptyList[Step2[I]]): Sequence2[I] = {
    def toSer[S](stepType: Step2.Type, steps: NonEmptyList[S], props: List[Prop[S]])(implicit ev: ClassTag[S]): SerSteps = {
      val empty = List.fill(props.length)(RunLength.empty[p.B forSome {val p: Prop[S]}])
      val runs  = steps.foldLeft(empty) { (lst, s) =>
        lst.fzipWith(props) { case (rl, p) =>
          (p.lens.get(s) :: rl.asInstanceOf[RunLength[p.B]])(p.eq)
        }
      }
      new SerSteps(stepType, runs.toNel.get)
    }

    @tailrec
    def serializeMatching[S](stepType: Step2.Type, steps: NonEmptyList[S], props: List[Prop[S]], rem: List[Step2[I]])(implicit ev: ClassTag[S]): (SerSteps, List[Step2[I]]) =
      rem match {
        case (h: S) :: t => serializeMatching[S](stepType, h <:: steps, props, t)
        case _           => (toSer[S](stepType, steps, props), rem)
      }

    @tailrec
    def go(nel: NonEmptyList[Step2[I]], res: List[SerSteps]): NonEmptyList[SerSteps] = {
      val (ss, remainder) = nel.head match {
        case s: BiasStep[I]    => serializeMatching(Bias,    s.wrapNel, implicitly[Describe[BiasStep[I]]].props,    nel.tail)
        case s: DarkStep[I]    => serializeMatching(Dark,    s.wrapNel, implicitly[Describe[DarkStep[I]]].props,    nel.tail)
        case s: GcalStep[I]    => serializeMatching(Gcal,    s.wrapNel, implicitly[Describe[GcalStep[I]]].props,    nel.tail)
        case s: ScienceStep[I] => serializeMatching(Science, s.wrapNel, implicitly[Describe[ScienceStep[I]]].props, nel.tail)
        case s: SmartStep[I]   => serializeMatching(Smart,   s.wrapNel, implicitly[Describe[SmartStep[I]]].props,   nel.tail)
      }

      remainder.toNel match {
        case None       => NonEmptyList.nel(ss, res)
        case Some(nel2) => go(nel2, ss :: res)
      }
    }

    new Sequence2[I](go(steps.reverse, Nil))
  }
}
