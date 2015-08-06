package edu.gemini.spModel.sequence

import scalaz._
import Scalaz._

/** Metadata describing the values of a property. */
trait Metadata[A] {
  def name: String
  def access: Metadata.Access
  def scope: Metadata.Scope

  /** A function that converts a value into a short value suitable for
    * displaying in a log, for example. */
  def log: A => String

  def serialize(a: A): String
  def deserialize(s: String): String \/ A
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

/** Convenience class for creating a `Metadata` instance for enum values. */
case class EnumMetadata[A](name: String, access: Metadata.Access, scope: Metadata.Scope, all: NonEmptyList[A], log: A => String) extends Metadata[A] {
  def serialize(a: A): String = log(a)
  def deserialize(s: String): String \/ A =
    all.list.find(a => log(a) === s) \/> s"$name not recognized: $s"
}

