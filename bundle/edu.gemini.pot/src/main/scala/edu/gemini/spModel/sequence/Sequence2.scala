package edu.gemini.spModel.sequence

import edu.gemini.spModel.sequence.Step2._

import scala.annotation.tailrec
import scala.language.existentials
import scala.reflect.ClassTag
import scalaz._
import Scalaz._

/** A sequence is logically a NonEmptyList[Step[I]], where I is some Instrument.
  * We don't directly store it that way for a couple of reasons:
  *
  * <ul>
  * <li> NonEmptyList is not Serializable and we require that sequences be
  *      serializable so that they may be transferred by TRPC.</li>
  * <li> It would be inefficient to store and transfer the expanded sequence in
  *      that way.</li>
  * </ul>
  *
  * Instead we break the sequence into chunks of SerSteps, where each chunk is
  * of the same Step type.  The sequence is guaranteed to have at least one
  * chunk of SerSteps but may have any number of trailing chunks.
  *
  * Each SerSteps is a compact representation of the properties associated with
  * the instrument and step type.  In particular, we store the first value of
  * the series of steps and then the only the differences for the remainder of
  * the steps (see RunLength).
  *
  * Note the internal representation isn't exposed so that there is no way to
  * create an invalid sequence.  To get the local view of NonEmptyList[Step[I]],
  * a `toSteps` method is provided.  To create a new sequence, use `fromSteps`.
  */
final class Sequence2[I] private (private val head: Sequence2.SerSteps, private val tail: List[Sequence2.SerSteps]) extends Serializable {

  private def this(sers: NonEmptyList[Sequence2.SerSteps]) = this(sers.head, sers.tail)

  /** Gets the listing of steps associated with this sequence. */
  def toSteps(implicit ev: Describe[I]): NonEmptyList[Step2[I]] =
    NonEmptyList.nel(head, tail).flatMap(_.toSteps)
}

object Sequence2 {

  // These type tags are used only in the deserialization process, so they are
  // made private and shouldn't escape the sequence implementation.
  private class SerSteps(stepType: Step2.Type, values: List[RunLength[_]]) extends Serializable {

    def toSteps[I: Describe]: NonEmptyList[Step2[I]] = {
      val rows = values.map(_.toList).transpose

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

  /** Creates a sequence from a non-empty list of steps. */
  def fromSteps[I: Describe](steps: NonEmptyList[Step2[I]]): Sequence2[I] = {
    def toSer[S](stepType: Step2.Type, steps: NonEmptyList[S], props: List[Prop[S]])(implicit ev: ClassTag[S]): SerSteps = {
      val empty = List.fill(props.length)(RunLength.empty[p.B forSome {val p: Prop[S]}])
      val runs  = steps.foldLeft(empty) { (lst, s) =>
        lst.fzipWith(props) { case (rl, p) =>
          (p.lens.get(s) :: rl.asInstanceOf[RunLength[p.B]])(p.eq)
        }
      }
      new SerSteps(stepType, runs)
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
