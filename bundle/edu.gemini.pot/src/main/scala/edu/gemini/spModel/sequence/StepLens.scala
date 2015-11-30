package edu.gemini.spModel.sequence

import scalaz.Scalaz._
import scalaz._

/** Support for getting and setting property values across multiple steps. */
final case class StepLens[A: Describe, I: Describe](prop: Prop[A], aLens: Step[I] @?> A) {
  val lens: Step[I] @?> prop.B = aLens >=> prop.lens.partial

  /** Gets all values, one per provided step. If the step type doesn't support
    * the property, the corresponding list element will be None. */
  def getAll(ss: List[Step[I]]): List[Option[prop.B]] =
    ss.map(lens.get)

  def getAll(seq: Sequence[I]): List[Option[prop.B]]  =
    getAll(seq.toSteps.list)

  def getAll(seq: Sequence[I], selected: Set[Int]): List[(Option[prop.B], Int)] =
    seq.toSteps.list.zipWithIndex.collect { case (step, i) if selected(i) =>
      (lens.get(step), i)
    }

  /** The common value if it is the same across all steps that support the
    * property. If two or more property supporting steps have different values
    * then None. */
  def getCommon(ss: List[Step[I]]): Option[prop.B] =
    getAll(ss).flatten.toSet.toList match {
      case pb :: Nil => Some(pb)
      case _         => None
    }

  def getCommon(seq: Sequence[I]): Option[prop.B] =
    getCommon(seq.toSteps.list)

  def getCommon(seq: Sequence[I], selected: Set[Int]): Option[prop.B] =
    getCommon(seq.toSteps.list.zipWithIndex.collect { case (step, i) if selected(i) => step })

  /** Sets the property value for all step types that support the property,
    * leaving those that do not unchanged. */
  def setAll(ss: List[Step[I]], pb: prop.B): List[Step[I]] =
    ss.map(s => lens.set(s, pb).getOrElse(s))

  def setAll(seq: Sequence[I], pb: prop.B): Sequence[I] =
    Sequence.fromSteps(setAll(seq.toSteps.list, pb).toNel.get)

  def setAll(seq: Sequence[I], selected: Set[Int], pb: prop.B): Sequence[I] = {
    val newSteps = seq.toSteps.list.zipWithIndex.map { case (step, i) =>
      if (selected(i)) lens.set(step, pb).getOrElse(step) else step
    }
    Sequence.fromSteps(newSteps.toNel.get)
  }
}