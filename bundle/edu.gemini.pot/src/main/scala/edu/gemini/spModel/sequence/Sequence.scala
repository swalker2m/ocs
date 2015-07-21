package edu.gemini.spModel.sequence

import scala.language.existentials
import scalaz._
import Scalaz._

/** A sequence is an initial step and a list of diffs. */
final case class Sequence[A] private (init: A, diffs: List[RunLength[_]]) {

  def toSteps(implicit ev: Describe[A]): NonEmptyList[A] = {
    val props = ev.props
    val rows  = diffs.map(_.toList).transpose

    val steps = rows.scanLeft(init) { (s, r) =>
      (s/:r.zip(props)) { case (s1, (a, p)) =>
        p.lens.set(s1, a.asInstanceOf[p.B])
      }
    }

    steps.toNel.get
  }
}

object Sequence {
  def fromSteps[A](steps: NonEmptyList[A])(implicit ev: Describe[A]): Sequence[A] = {
    val props = ev.props
    val rls   = List.fill(props.length)(RunLength.empty[p.B forSome { val p : Prop[A]}])

    Sequence(steps.head, (steps.tail:\rls) { (s,lst) =>
      lst.fzipWith(props) { case (rl, p) => (p.lens.get(s) :: rl.asInstanceOf[RunLength[p.B]])(p.eq) }
    })
  }
}

/** A sequence is an initial value and a list of transitions. */
//final case class Sequence[I](init: Step[I], xs: List[Step[I] => Step[I]]) {
//  def toSteps: NonEmptyList[Step[I]] =
//    xs.scanLeft(init)((s, x) => x(s)).toNel.get
//}

//object Sequence {
//  def fromSteps[I](steps: NonEmptyList[Step[I]])(implicit ev: Describe[I]): Sequence[I] = {
//    val dsi = implicitly[Describe[Step[I]]]
//    Sequence(steps.head, steps.list.fzipWith(steps.tail)(dsi.diff))
//  }
//}

/*
final case class Sequence[I](init: Step[I], diffs: List[List[(Int, Any)]]) {
  def toSteps(implicit ev: Describe[I]): NonEmptyList[Step[I]] = {
    val props = implicitly[Describe[Step[I]]].props.toVector.lift
    diffs.scanLeft(init) { (s, ds) =>
      (s/:ds) { case (s0, (i,a)) =>
        props(i).fold(s0) { p => p.lens.set(s0, a.asInstanceOf[p.B]) }
      }
    }.toNel.get
  }
}

object Sequence {
  def fromSteps[I: Describe](steps: NonEmptyList[Step[I]]): Sequence[I] = {
    val props = implicitly[Describe[Step[I]]].props

    val diffs = steps.list.fzipWith(steps.tail) { (s0, s1) =>
      (List.empty[(Int, Any)]/:props.zipWithIndex) { case (lst, (p, i)) =>
        if (p.propEqual(s0, s1)) lst else (i, p.lens.get(s1)) :: lst
      }
    }

    Sequence(steps.head, diffs)
  }
}
*/
/*
final case class Sequence[I] private (init: Step[I], diffs: List[RunLength[_]]) {

  def toSteps(implicit ev: Describe[I]): NonEmptyList[Step[I]] = {
    val props = implicitly[Describe[Step[I]]].props
    val rows  = diffs.map(_.toList).transpose

    val steps = rows.scanLeft(init) { (s, r) =>
      (s/:r.zip(props)) { case (s1, (a, p)) =>
        p.lens.set(s1, a.asInstanceOf[p.B])
      }
    }

    steps.toNel.get
  }
}

object Sequence {
  def fromSteps[I: Describe](steps: NonEmptyList[Step[I]]): Sequence[I] = {

    val props = implicitly[Describe[Step[I]]].props
    val rls   = List.fill(props.length)(RunLength.empty[p.B forSome { val p : Prop[Step[I]]}])

    Sequence(steps.head, (steps.tail:\rls) { (s,lst) =>
      lst.fzipWith(props) { case (rl, p) => (p.lens.get(s) :: rl.asInstanceOf[RunLength[p.B]])(p.eq) }
    })
  }
}
*/

