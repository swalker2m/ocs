package edu.gemini.spModel.sequence

import edu.gemini.spModel.core.AngleSyntax._
import edu.gemini.spModel.core.{OffsetQ, OffsetP, Offset}
import edu.gemini.spModel.obsclass.ObsClass

import java.io.{ObjectInputStream, ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream}

import scalaz._
import Scalaz._

/**
 *
 */
object Sample extends App {
/*
  def showStep(s: Step[Flamingos2]): String =
    s"""
       |------------------------------
       |Telescope
       |  p = ${s.telescope.offset.p.shows}
       |  q = ${s.telescope.offset.q.shows}
       |
       |Instrument
       |  filter    = ${s.instrument.filter}
       |  disperser = ${s.instrument.disperser}
     """.stripMargin

  def showSequence(s: Sequence[Flamingos2]): String =
    s.steps.map(showStep).list.mkString("\n")

  def printSequence(s: Sequence[Flamingos2]): Unit =
    println(showSequence(s))
*/

  def serialize[A](a: A): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(a)
    oos.close()
    baos.toByteArray
  }

  def deserialize[A](a: Array[Byte]): A = {
    val bais = new ByteArrayInputStream(a)
    val ois  = new ObjectInputStream(bais)
    val f2   = ois.readObject().asInstanceOf[A]
    ois.close()
    f2
  }

//  val dsi = implicitly[Describe[Step[F2]]]
//  val s0 = Step(Telescope(OffsetP.Zero,   40.0.arcsecs[OffsetQ]),   F2(F2.Filter.J, F2.Disperser.None))
//  val s1 = Step(Telescope(OffsetP.Zero, (-40.0).arcsecs[OffsetQ]),  F2(F2.Filter.J, F2.Disperser.None))
//  println(deserialize[Step[F2] => Step[F2]](serialize(dsi.diff(s0, s1))))

  val steps = NonEmptyList(
    Step(Telescope(OffsetP.Zero,   40.0.arcsecs[OffsetQ]),  F2(F2.Filter.J, F2.Disperser.None)),
    Step(Telescope(OffsetP.Zero, (-40.0).arcsecs[OffsetQ]), F2(F2.Filter.J, F2.Disperser.None)),
    Step(Telescope(OffsetP.Zero, (-40.0).arcsecs[OffsetQ]), F2(F2.Filter.H, F2.Disperser.None)),
    Step(Telescope(OffsetP.Zero,   40.0.arcsecs[OffsetQ]),  F2(F2.Filter.J, F2.Disperser.None))
  )

  val seq = Sequence.fromSteps(steps)

  val steps2 = deserialize[Sequence[Step[F2]]](serialize(seq)).toSteps
//  val steps2 = seq.toSteps

  steps2.list.foreach(println)
}
