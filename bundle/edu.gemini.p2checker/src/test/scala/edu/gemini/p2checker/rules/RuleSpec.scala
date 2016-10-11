package edu.gemini.p2checker.rules

import edu.gemini.spModel.data.config.{DefaultParameter, DefaultSysConfig}
import edu.gemini.spModel.seqcomp.{SeqConfigObsBase, SeqConfigNames}
import org.specs2.matcher.MatchResult

import java.beans.PropertyDescriptor
import java.util.UUID

import edu.gemini.p2checker.api.{IRule, ObservationElements}
import edu.gemini.pot.sp.{ISPObservation, SPComponentType}
import edu.gemini.pot.util.POTUtil
import edu.gemini.spModel.core.SPProgramID
import edu.gemini.spModel.data.ISPDataObject
import org.specs2.mutable.Specification

import scala.collection.JavaConverters._
import scalaz.syntax.id._

/**
 * Generic test bed for arbitrary P2 checker rule tests.
 */
abstract class RuleSpec extends Specification {

  // the ruleset to be tested
  def ruleSet: IRule

  // configurable test setup, feel free to adapt as needed for future tests
  def setup[I <: ISPDataObject](instrument: SPComponentType, programId: String = "")(mod: I => Unit): ISPObservation = {
    val f = POTUtil.createFactory(UUID.randomUUID())
    val p = f.createProgram(null, SPProgramID.toProgramID(programId))
    val o = f.createObservation(p, null) <| p.addObservation
    val i = f.createObsComponent(p, instrument, null) <| o.addObsComponent

    // Modify the instrument's data object as needed
    val dataObj = i.getDataObject
    mod(dataObj.asInstanceOf[I])
    i.setDataObject(dataObj)

    // This is our observation..
    o
  }

  def setupWithSequence[I <: ISPDataObject, S <: SeqConfigObsBase](instrument: SPComponentType, sequence: SPComponentType)(init: (PropertyDescriptor, List[Any])*): ISPObservation = {
    val f = POTUtil.createFactory(UUID.randomUUID())
    val p = f.createProgram(null, null)
    val o = f.createObservation(p, null)              <| p.addObservation
    o.addObsComponent(f.createObsComponent(p, instrument, null))
    val s = f.createSeqComponent(p, sequence, null)   <| o.getSeqComponent.addSeqComponent

    // Modify the sys config of the instrument iterator
    val sdbj = s.getDataObject.asInstanceOf[S]
    val sc   = new DefaultSysConfig(SeqConfigNames.INSTRUMENT_CONFIG_NAME)

    init.foreach { case (prop, vals) =>
      sc.putParameter(DefaultParameter.getInstance(prop, vals.asJava))
    }
    sdbj.setSysConfig(sc)

    s.setDataObject(sdbj)

    o
  }

  // exercise the defined rule set
  def executeRules(o: ISPObservation): List[String] = {
    val oe = new ObservationElements(o)
    ruleSet.check(oe).getProblems.asScala.map(_.getId).toList
  }

  // check if expected errors and warnings are part of the result
  def expectAllOf(ids: String*)(o: ISPObservation): MatchResult[List[String]] =
    executeRules(o)  must containAllOf(ids)

  // check if expected errors and warnings are part of the result
  def expectNoneOf(ids: String*)(o: ISPObservation): MatchResult[List[String]] =
    executeRules(o) must not(containAnyOf(ids))

}
