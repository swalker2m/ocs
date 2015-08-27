package jsky.app.ot.editor.sequence

import edu.gemini.spModel.sequence.{StepLens, Step, PropGroup, Describe, Sequence}

import javax.swing.table.AbstractTableModel

import scalaz._
import Scalaz._

/**
 *
 */
class SequenceTableModel[I: Describe](seq: Sequence[I]) extends AbstractTableModel {
  private val stepList: List[Step[I]]            = seq.toSteps.list

  private val groups: Vector[PropGroup[_, I]]    = PropGroup.groups(stepList).toVector

  private val stepLenses: Vector[StepLens[_, I]] = groups.flatMap { g =>
    val sl: Vector[StepLens[_, I]] = g.stepLenses.toVector
    sl.filter { sl => !sl.hasCommon(stepList) }
  }

  val data: Vector[Vector[Option[_]]] =
    (0 until stepList.size).toVector.map(Some(_)) +: stepLenses.map { sl =>
      sl.getAll(stepList).toVector
    }

  val colNames: Vector[String] =
    "#" +: stepLenses.map(_.prop.meta.attrs.label.name)

  override def getRowCount: Int =
    data.headOption.map(_.size).orZero

  override def getColumnCount: Int =
    data.size

  override def getColumnName(col: Int): String =
    colNames(col)

  override def getValueAt(row: Int, col: Int): Object =
    data(col)(row)

  override def isCellEditable(row: Int, col: Int): Boolean =
    col == 0

  def format(col: Int, a: Any): String = {
    val p = stepLenses(col-1).prop

    a match {
      case Some(pb: p.B) => p.meta.show(pb)
      case _             => ""
    }
  }
}
