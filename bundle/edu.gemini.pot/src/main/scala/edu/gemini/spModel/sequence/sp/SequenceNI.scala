package edu.gemini.spModel.sequence.sp

import edu.gemini.pot.sp.{ISPFactory, ISPNode, ISPNodeInitializer}

final class SequenceNI extends ISPNodeInitializer{

  def initNode(factor: ISPFactory, node: ISPNode): Unit = {
    node.setDataObject(new SequenceDO())
  }

  def updateNode(node: ISPNode): Unit = ()
}
