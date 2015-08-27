package jsky.app.ot.editor.sequence

import edu.gemini.pot.sp.{SPNodeKey, ISPSequence}
import edu.gemini.shared.gui.TableUtil
import edu.gemini.spModel.rich.pot.sp.SpNodeKeyEqual
import edu.gemini.spModel.sequence.{Describe, F2, Sequence, F2Sequence, InstrumentSequence}
import edu.gemini.spModel.sequence.sp.SequenceDO

import jsky.app.ot.editor.OtItemEditor
import jsky.app.ot.util.Resources

import java.util.EventObject
import javax.swing.event.{ChangeEvent, CellEditorListener}
import javax.swing.{BorderFactory, JTable, JPanel}
import javax.swing.table.TableCellEditor

import scala.swing.event.TableRowsSelected
import scala.swing._

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

      class StepPanel(stepIndex: Int) extends BorderPanel {
        val menu = new Menu(s"${stepIndex + 1}")
        menu.opaque    = false
        menu.focusable = false
        menu.icon      = Resources.getIcon("eclipse/menu-trimmed.gif")
        menu.horizontalTextPosition = Alignment.Left

        val bar       = new MenuBar()
        bar.opaque    = false
        bar.focusable = false
        bar.border    = BorderFactory.createEmptyBorder()
        bar.contents += Swing.HGlue
        bar.contents += menu

        opaque      = false
        layout(bar) = BorderPanel.Position.Center
      }


      final class StepEditor(stepIndex: Int) extends StepPanel(stepIndex) with TableCellEditor {
        private var cels = List.empty[CellEditorListener]

        menu.contents += new MenuItem(Action("Duplicate Step") {
          val steps = s.toSteps.list
          val step  = steps(stepIndex)
          commit(construct(Sequence.fromSteps(steps.patch(stepIndex, List(step), 0).toNel.get)))
        })

        override def getTableCellEditorComponent(table: JTable, value: scala.Any, isSelected: Boolean, row: Int, column: Int): java.awt.Component = {
          this.peer
        }

        override def addCellEditorListener(cel: CellEditorListener): Unit = {
          cels = cel :: cels
        }

        override def removeCellEditorListener(cel: CellEditorListener): Unit = {
          cels = cels.filterNot(_ == cel)
        }

        override def getCellEditorValue: Object                      = ""
        override def shouldSelectCell(anEvent: EventObject): Boolean = false
        override def isCellEditable(anEvent: EventObject): Boolean   = true

        override def stopCellEditing(): Boolean = {
          fireEditingStopped()
          true
        }

        override def cancelCellEditing(): Unit = {
          fireEditingCancelled()
        }

        private def fireEditingStopped(): Unit = {
          val evt = new ChangeEvent(this)
          cels.foreach(_.editingStopped(evt))
        }

        private def fireEditingCancelled(): Unit = {
          val evt = new ChangeEvent(this)
          cels.foreach(_.editingCanceled(evt))
        }
      }

      object optionRenderer extends Label {
        def prepare(stm: SequenceTableModel[I], c: Int, a: Any): Unit = {
          text   = stm.format(c, a)
          border = BorderFactory.createEmptyBorder(0, 2, 0, 2)
        }
      }

      val tab   = new Table() {
        override def rendererComponent(sel: Boolean, foc: Boolean, row: Int, col: Int) = {
          val v = model.getValueAt(peer.convertRowIndexToModel(row),
                                   peer.convertColumnIndexToModel(col))

          val stm = model.asInstanceOf[SequenceTableModel[I]]

          val comp = (col, v) match {
            case (0, Some(step: Int)) => new StepPanel(step)
            case _                    => optionRenderer.prepare(stm, col, v)
                                         optionRenderer
          }

          comp.opaque     = true
          comp.background = if (sel) selectionBackground else background
          comp
        }

        override def editor(row: Int, col: Int): TableCellEditor = {
          col match {
            case 0 => new StepEditor(row)
            case _ => null
          }
        }
      }
      tab.model                  = new SequenceTableModel(s)
      tab.autoResizeMode         = Table.AutoResizeMode.Off
      tab.selection.elementMode  = Table.ElementMode.Row
      tab.selection.intervalMode = Table.IntervalMode.MultiInterval
      tab.selection.rows        += 0
      TableUtil.minimizeColumnWidths(tab.peer, 4)

      val sp    = new ScrollPane(tab)
      pan.layout(sp) = BorderPanel.Position.Center

      val ps = new PropSheet(s, tab.selection.rows.toSet, construct, commit)
      pan.layout(ps) = BorderPanel.Position.South

      pan.listenTo(tab.selection)
      pan.reactions += {
        case TableRowsSelected(_, _, false) =>
          val comps = pan.layout.collect {
            case (comp, BorderPanel.Position.South) => comp
          }

          pan.layout --= comps

          val ps = new PropSheet(s, tab.selection.rows.toSet, construct, commit)
          pan.layout(ps) = BorderPanel.Position.South
          pan.revalidate()
          pan.repaint()
      }
    }


    getDataObject.seq.foreach {
      case F2Sequence(s) => init(s, (s: Sequence[F2]) => F2Sequence(s))
      case _             => // do nothing
    }
  }

  override def getWindow: JPanel = pan.peer
}
