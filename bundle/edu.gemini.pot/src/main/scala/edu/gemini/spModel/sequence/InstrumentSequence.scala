package edu.gemini.spModel.sequence

/**
 *
 */
sealed trait InstrumentSequence

final case class F2Sequence(s: Sequence[F2]) extends InstrumentSequence

