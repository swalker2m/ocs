package edu.gemini.spModel.sequence

import edu.gemini.spModel.`type`.LoggableSpType

import scalaz._
import Scalaz._

/** Metadata describing the values of a property. */
sealed trait Metadata[A] {
  def name: String
  def access: Metadata.Access
  def scope: Metadata.Scope

  def show(a: A): String = a.toString
  def read(s: String): String \/ A
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
}

case class EnumMetadata[A](
  name: String,
  access: Metadata.Access,
  scope: Metadata.Scope,
  values: NonEmptyList[A]) extends Metadata[A] {

  override def show(a: A): String = a match {
    case l: LoggableSpType => l.logValue
    case _                 => a.toString
  }

  override def read(s: String): String \/ A =
    values.list.find(v => show(v) === s) \/> s"$name `$s` not recognized"
}

object EnumMetadata {
  def fromJava[A <: java.lang.Enum[A]](n: String, a: Metadata.Access, s: Metadata.Scope, c: Class[A]): Metadata[A] = {
    val values = c.getEnumConstants
    EnumMetadata[A](n, a, s, NonEmptyList.nel(values.head, values.tail.toList))
  }
}

case class BooleanMetadata(
  name: String,
  access: Metadata.Access,
  scope: Metadata.Scope) extends Metadata[Boolean] {

  override def read(s: String): String \/ Boolean =
    s match {
      case "true"  => true.right
      case "false" => false.right
      case _       => s"$name value must be `true` or `false`, not `$s`".left
    }
}

case class TextMetadata[A](
  name: String,
  access: Metadata.Access,
  scope: Metadata.Scope,
  toText: A => String,
  fromText: String => String \/ A) extends Metadata[A] {

  override def show(a: A): String           = toText(a)
  override def read(s: String): String \/ A = fromText(s)
}


