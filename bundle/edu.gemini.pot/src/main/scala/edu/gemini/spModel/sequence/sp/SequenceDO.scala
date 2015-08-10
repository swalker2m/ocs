package edu.gemini.spModel.sequence.sp

import edu.gemini.pot.sp.SPComponentType
import edu.gemini.spModel.data.AbstractDataObject
import edu.gemini.spModel.pio.{PioFactory, ParamSet}
import edu.gemini.spModel.sequence.Sequence

final class SequenceDO extends AbstractDataObject {
  val SpType  = SPComponentType.SEQUENCE_NODE
  val Version = "2016A-1"

  private var sequence: Option[Sequence[_]] = None

  setTitle("Sequence")
  setType(SpType)
  setVersion(Version)

  def this(paramSet: ParamSet) = {
    this()
    setParamSet(paramSet)
  }

  def seq: Option[Sequence[_]] = sequence

  def seq_=(s: Option[Sequence[_]]): Unit = {
    sequence = s
  }
}
