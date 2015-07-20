package edu.gemini.spModel.sequence

import scalaz._
import Scalaz._

/** A sequence is an initial value and a list of transitions. */
final case class Sequence[I](init: Step[I], xs: List[Step[I] => Step[I]]) {
  def toSteps: NonEmptyList[Step[I]] =
    xs.scanLeft(init)((s, x) => x(s)).toNel.get
}

object Sequence {
  def fromSteps[I](steps: NonEmptyList[Step[I]])(implicit ev: Describe[I]): Sequence[I] = {
    val dsi = implicitly[Describe[Step[I]]]
    Sequence(steps.head, steps.list.fzipWith(steps.tail)(dsi.diff))
  }
}