package jsky.app.ot.editor.sequence

import edu.gemini.spModel.sequence.Metadata.Label
import edu.gemini.spModel.sequence.{SmartCal, GcalUnit, Telescope, Prop, StepLens, Step, PropGroup, Describe, Sequence}
import jsky.app.ot.editor.sequence.SequenceTableModel.{TypeColumn, IndexColumn, PropertyColumn, Column}
import jsky.app.ot.util.OtColor

import java.awt.Color
import javax.swing.table.AbstractTableModel

import scala.swing.Alignment
import scalaz._
import Scalaz._


object SequenceTableModel {

  sealed trait Column[A, I] {
    def label: Label

    def name: String = label.name

    def color: Color = label.parent match {
      case Some(Telescope.Lab) => OtColor.SKY
      case Some(GcalUnit.Lab) => OtColor.BANANA
      case Some(SmartCal.Lab) => OtColor.BANANA
      case Some(_) => OtColor.HONEY_DEW
      case _ => Color.WHITE
    }

    def align: Alignment.Value = Alignment.Center

    def value(row: Int, step: Step[I]): Option[A]

    def format(a: Any): String = a.toString
  }

  final case class IndexColumn[I] extends Column[Int, I] {
    val label: Label = Label("#")

    override def align: Alignment.Value = Alignment.Right

    def value(row: Int, step: Step[I]): Option[Int] =
      Some(row + 1)
  }

  final case class TypeColumn[I] extends Column[Step.Type, I] {
    val label: Label = Label("Type")

    def value(row: Int, step: Step[I]): Option[Step.Type] =
      Some(step.stepType)
  }

  final case class PropertyColumn[B, I: Describe](prop: Prop[_], lens: Step[I] @?> B) extends Column[B, I] {
    val label = prop.meta.attrs.label

    def value(row: Int, step: Step[I]): Option[B] = lens.get(step)

    override def format(a: Any): String = a match {
      // ugh
      case pb: prop.B => prop.meta.show(pb)
      case _ => ""
    }
  }

  case class SequenceState[I: Describe](seq: Sequence[I], rows: Vector[Step[I]], columns: Vector[Column[_, I]]) {
    def set(p: Prop[_], selected: Set[Int])(v: p.B): SequenceState[I] = ???

  }

  object SequenceState {
    def apply[I: Describe](seq: Sequence[I]): SequenceState[I] = {
      val stepList: List[Step[I]]          = seq.toSteps.list
      val pGroups: Vector[PropGroup[_, I]] = PropGroup.groups(stepList).toVector
      val pCols: Vector[Column[_, I]]      = pGroups.flatMap { grp =>
        grp.stepLenses.toVector.collect {
          case sl if !sl.hasCommon(stepList) => PropertyColumn[sl.prop.B, I](sl.prop, sl.lens)
        }
      }
      val cols: Vector[Column[_, I]]       = IndexColumn[I] +: TypeColumn[I] +: pCols

      SequenceState(seq, stepList.toVector, cols)
    }
  }
}

class SequenceTableModel[I: Describe](seq: Sequence[I]) extends AbstractTableModel {


  private val stepList: List[Step[I]]            = seq.toSteps.list

  private val stepVector: Vector[Step[I]] = stepList.toVector

  private val groups: Vector[PropGroup[_, I]]    = PropGroup.groups(stepList).toVector

//  private val stepLenses: Vector[StepLens[_, I]] = groups.flatMap { g =>
//    val sl: Vector[StepLens[_, I]] = g.stepLenses.toVector
//    sl.filter { sl => !sl.hasCommon(stepList) }
//  }

  private val propColumns: Vector[Column[_, I]] = groups.flatMap { grp =>
    grp.stepLenses.toVector.collect {
      case sl if !sl.hasCommon(stepList) => PropertyColumn[sl.prop.B, I](sl.prop, sl.lens)
    }
  }

  val columns: Vector[Column[_, I]] =
    IndexColumn[I] +: TypeColumn[I] +: propColumns

  override def getRowCount: Int =
    stepVector.size

  override def getColumnCount: Int =
    columns.size

  override def getColumnName(col: Int): String =
    columns(col).name

  override def getValueAt(row: Int, col: Int): Object =
    columns(col).value(row, stepVector(row))

  override def isCellEditable(row: Int, col: Int): Boolean =
    col == 0
}
