package edu.gemini.spModel.sequence

import edu.gemini.spModel.`type`.LoggableSpType

import scalaz._
import Scalaz._

/** Metadata describing the values of a property. */
sealed trait Metadata[A] {
  def attrs: Metadata.Attrs
  def show: A => String
  def read: String => String \/ A
}

object Metadata {
  sealed trait Access
  object Access {
    case object Engineering extends Access
    case object Science     extends Access
  }

  sealed trait Scope
  object Scope {
    case object Global     extends Scope
    case object SingleStep extends Scope
  }

  case class Attrs(name: String, label: String, access: Access, scope: Scope)
}

case class EnumMetadata[A](attrs: Metadata.Attrs, values: NonEmptyList[A]) extends Metadata[A] {
  override val show: A => String = {
    case l: LoggableSpType => l.logValue
    case a                 => a.toString
  }

  override val read: String => String \/ A = (s: String) =>
    values.list.find(v => show(v) === s) \/> s"${attrs.label} `$s` not recognized"
}

object EnumMetadata {
  def fromJava[A <: java.lang.Enum[A]](attrs: Metadata.Attrs, c: Class[A]): Metadata[A] = {
    val values = c.getEnumConstants
    EnumMetadata[A](attrs, NonEmptyList.nel(values.head, values.tail.toList))
  }
}

case class BooleanMetadata(attrs: Metadata.Attrs) extends Metadata[Boolean] {
  override val show: Boolean => String = _.toString

  override val read: String => String \/ Boolean = {
    case "true"  => true.right
    case "false" => false.right
    case s       => s"${attrs.label} value must be `true` or `false`, not `$s`".left
  }
}

case class TextMetadata[A](
  attrs: Metadata.Attrs,
  units: Option[String],
  show: A => String,
  read: String => String \/ A) extends Metadata[A]


