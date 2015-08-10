package jsky.app.ot.viewer.action

import edu.gemini.pot.sp.ISPObservation
import jsky.app.ot.editor.sequence.SequenceEditor
import jsky.app.ot.nsp.SPTreeEditUtil
import jsky.app.ot.viewer.SPViewer

import java.awt.event.ActionEvent

/**
 *
 */
final class AddSequenceAction(v: SPViewer) extends AbstractViewerAction(v, "Sequence") {
  override def computeEnabledState(): Boolean = {
    SequenceEditor.NewSequenceSupport && {
      Option(getContextNode(classOf[ISPObservation])).exists { o =>
        val seq = v.getFactory.createSequence(getProgram, AbstractViewerAction.BOGUS_KEY)
        SPTreeEditUtil.isOkayToAdd(getProgram, seq, o, v.getNode)
      }
    }
  }

  override def actionPerformed(e: ActionEvent): Unit = {
    val obs = getContextNode(classOf[ISPObservation])
    val seq = v.getFactory.createSequence(getProgram, null)
    obs.setSequence(seq)
  }
}
