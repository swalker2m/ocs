package jsky.app.ot.editor.sequence

import edu.gemini.pot.sp.ISPSequence
import edu.gemini.spModel.sequence.{Describe, Metadata, Prop, Step, Sequence}
import edu.gemini.spModel.sequence.sp.SequenceDO
import jsky.app.ot.editor.OtItemEditor

import javax.swing.{JPanel, BorderFactory}

import scala.swing.{Component, Label, GridBagPanel}

object SequenceEditor {
  var NewSequenceSupport = false
}

class SequenceEditor extends OtItemEditor[ISPSequence, SequenceDO] {


  val pan = new GridBagPanel {
    border = BorderFactory.createCompoundBorder(
      BorderFactory.createEmptyBorder(10, 10, 10, 10),
      BorderFactory.createLoweredSoftBevelBorder())

    layout(new Label("F2 Sequence")) = new Constraints {

    }
  }

  override protected def init(): Unit = {
//    getDataObject.seq.fold()
  }

  private def init(s: Sequence[_]): Unit = {

  }

  override def getWindow: JPanel = pan.peer
}
