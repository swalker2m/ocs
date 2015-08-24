package jsky.app.ot.editor.sequence

import edu.gemini.spModel.sequence.{PropGroup, Describe, Sequence}

import javax.swing.table.AbstractTableModel

import scalaz._
import Scalaz._

/**
 *
 */
class SequenceTableModel[I: Describe](seq: Sequence[I]) extends AbstractTableModel {
  private val stepList = seq.toSteps.list
  private val groups   = PropGroup.groups(stepList).toVector

  val data: Vector[(String, Vector[Option[_]])] =
    ("#", (1 to stepList.size).toVector.map(Some(_))) +: groups.flatMap { g =>
      g.stepLenses.toVector.collect {
        case sl if sl.hasCommon(stepList) =>
          (sl.prop.meta.attrs.label.name, sl.getAll(stepList).toVector)
      }
    }

  val colNames: Vector[String] =
    data.map(_._1)

  override def getRowCount: Int =
    data.headOption.map { case (_, v) => v.size }.orZero

  override def getColumnCount: Int =
    data.size

  override def getColumnName(col: Int): String =
    colNames(col)

  override def getValueAt(row: Int, col: Int): Object =
    data(col)._2(row)
}
