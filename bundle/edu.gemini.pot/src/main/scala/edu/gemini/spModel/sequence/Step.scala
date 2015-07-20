package edu.gemini.spModel.sequence


final case class Step[I](telescope: Telescope, instrument: I)

object Step {
  // A step is describable if the instrument is describable.
  implicit def describeStep[I: Describe]: Describe[Step[I]] =
    Describe[(Telescope, I)].xmap[Step[I]](
      t => Step(t._1, t._2),
      s => (s.telescope, s.instrument)
    )
}