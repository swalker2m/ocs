package edu.gemini.spModel.sequence.sp

import edu.gemini.pot.sp.SPComponentType
import edu.gemini.spModel.data.AbstractDataObject
import edu.gemini.spModel.pio.ParamSet
import edu.gemini.spModel.sequence.InstrumentSequence

final class SequenceDO extends AbstractDataObject {
  val SpType  = SPComponentType.SEQUENCE_NODE
  val Version = "2016A-1"

  private var sequence: Option[InstrumentSequence] = None

  setTitle("Sequence")
  setType(SpType)
  setVersion(Version)

  def this(paramSet: ParamSet) = {
    this()
    setParamSet(paramSet)
  }

  def seq: Option[InstrumentSequence] = sequence

  def seq_=(s: Option[InstrumentSequence]): Unit = {
    sequence = s
  }
}
