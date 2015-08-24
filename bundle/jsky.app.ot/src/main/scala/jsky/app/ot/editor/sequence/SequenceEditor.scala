package jsky.app.ot.editor.sequence

import edu.gemini.pot.sp.{SPNodeKey, ISPSequence}
import edu.gemini.spModel.rich.pot.sp.SpNodeKeyEqual
import edu.gemini.spModel.sequence.{Describe, F2, Sequence, F2Sequence, InstrumentSequence}
import edu.gemini.spModel.sequence.sp.SequenceDO

import jsky.app.ot.editor.OtItemEditor

import javax.swing.{JTable, JPanel}
import javax.swing.table.DefaultTableCellRenderer

import scala.collection.JavaConverters._
import scala.swing.Table.{LabelRenderer, AbstractRenderer}
import scala.swing.{Label, Table, ScrollPane, BorderPanel}

import scalaz._
import Scalaz._

object SequenceEditor {
  var NewSequenceSupport = false
}

class SequenceEditor extends OtItemEditor[ISPSequence, SequenceDO] {

  val pan = new BorderPanel

  override protected def init(): Unit = {
    def curKey: SPNodeKey = getContextObservation.getNodeKey

    // Text widgets fire value changed events when they lose focus.  If you
    // click on a different sequence node, commit will be called with the
    // provided sequence which will be for the wrong node.
    val editKey = curKey

    def commit(is: InstrumentSequence): Unit =
      if (editKey === curKey) {
        val dob = getDataObject
        dob.seq = Some(is)
        apply()
      }

    def init[I: Describe](s: Sequence[I], construct: Sequence[I] => InstrumentSequence): Unit = {
      pan.layout.clear()

      object optionRenderer extends Label {
        def prepare(o: AnyRef): Unit =
          text = o match {
            case Some(v) => v.toString
            case None    => ""
            case _       => o.toString
          }
      }

      val tab   = new Table() {
        override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
          val v = model.getValueAt(peer.convertRowIndexToModel(row),
                                   peer.convertColumnIndexToModel(col))

          optionRenderer.prepare(v)
          optionRenderer
        }
      }
      tab.model = new SequenceTableModel(s)

      val sp    = new ScrollPane(tab)
      pan.layout(sp) = BorderPanel.Position.Center

      val ps = new PropSheet(s, Set(0), construct, commit)
      pan.layout(ps) = BorderPanel.Position.South
    }


    getDataObject.seq.foreach {
      case F2Sequence(s) => init(s, (s: Sequence[F2]) => F2Sequence(s))
      case _             => // do nothing
    }
  }

  override def getWindow: JPanel = pan.peer
}
