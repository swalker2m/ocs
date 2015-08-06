package edu.gemini.spModel.sequence


import edu.gemini.spModel.core.AngleSyntax._
import edu.gemini.spModel.core.{OffsetQ, OffsetP}

import java.io.{ObjectInputStream, ByteArrayInputStream, ByteArrayOutputStream, ObjectOutputStream}

import scalaz._
import Scalaz._

object Sample extends App {
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

  val f2_J = F2(F2.Filter.J, F2.Disperser.None)
  val f2_H = F2(F2.Filter.H, F2.Disperser.None)

  val nod_A = Telescope(OffsetP.Zero,   40.0.arcsecs[OffsetQ])
  val nod_B = Telescope(OffsetP.Zero, (-40.0).arcsecs[OffsetQ])

  val steps = NonEmptyList(
    DarkStep(    f2_J),
    ScienceStep( f2_J, nod_A),
    ScienceStep( f2_J, nod_B),
    ScienceStep( f2_J, nod_B),
    ScienceStep( f2_J, nod_A),
    SmartStep(   f2_J, SmartStep.Type.Flat),
    ScienceStep( f2_H, nod_A),
    ScienceStep( f2_H, nod_B),
    ScienceStep( f2_H, nod_B),
    ScienceStep( f2_H, nod_A),
    SmartStep(   f2_H, SmartStep.Type.Flat)
  )

  val seq = Sequence.fromSteps(steps)

//  val steps2 = seq.toSteps
  val steps2 = deserialize[Sequence[F2]](serialize(seq)).toSteps

//  steps2.list.foreach(println)

  steps2.list.foreach { step =>
    println(step.shows)
  }
}
