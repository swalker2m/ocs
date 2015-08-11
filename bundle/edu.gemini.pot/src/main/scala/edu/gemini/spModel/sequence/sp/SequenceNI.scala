package edu.gemini.spModel.sequence.sp

import edu.gemini.pot.sp.{ISPFactory, ISPNode, ISPNodeInitializer}
import edu.gemini.spModel.sequence.{F2, Sequence}

final class SequenceNI extends ISPNodeInitializer{

  def initNode(factor: ISPFactory, node: ISPNode): Unit = {
    val dob = new SequenceDO()
    dob.seq = Some(Sequence.initScience[F2])
    node.setDataObject(dob)
  }

  def updateNode(node: ISPNode): Unit = ()
}
