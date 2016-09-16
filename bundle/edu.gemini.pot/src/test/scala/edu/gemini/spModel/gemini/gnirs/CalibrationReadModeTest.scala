package edu.gemini.spModel.gemini.gnirs

import edu.gemini.pot.sp.{ISPSeqComponent, SPComponentType}
import edu.gemini.spModel.config.ConfigBridge
import edu.gemini.spModel.config.map.ConfigValMapInstances
import edu.gemini.spModel.config2.ConfigSequence
import edu.gemini.spModel.gemini.calunit.CalUnitParams.{Diffuser, Filter, Shutter, Lamp}
import edu.gemini.spModel.gemini.calunit.smartgcal.CalibrationProviderHolder
import edu.gemini.spModel.gemini.gnirs.GNIRSParams.ReadMode
import ReadMode.{BRIGHT, VERY_FAINT}
import edu.gemini.spModel.gemini.seqcomp.{SeqRepeatSmartGcalObs, CalImpl, TestCalibrationProvider}
import edu.gemini.spModel.test.InstrumentSequenceTestBase

import org.junit.{Before, Test}
import org.junit.Assert._

import java.util

class CalibrationReadModeTest extends InstrumentSequenceTestBase[InstGNIRS, SeqConfigGNIRS] {

  val cal = CalImpl(Set(Lamp.arcLamps().get(0)), Shutter.OPEN, Filter.ND_10, Diffuser.IR, 1, 1.0, 1, arc = true)

  CalibrationProviderHolder.setProvider(new TestCalibrationProvider(List(cal)))

  override def getObsCompSpType: SPComponentType =
    InstGNIRS.SP_TYPE

  override def getSeqCompSpType: SPComponentType =
    SeqConfigGNIRS.SP_TYPE

  private def configSeq: ConfigSequence =
    ConfigBridge.extractSequence(getObs, null, ConfigValMapInstances.IDENTITY_MAP)

  private def readModeAt(step: Int): ReadMode =
    configSeq.getItemValue(step, GNIRSConstants.READ_MODE_KEY).asInstanceOf[ReadMode]

  private def readModes: List[ReadMode] = {
    val rs = configSeq.getItemValueAtEachStep(GNIRSConstants.READ_MODE_KEY)
    rs.toList.map { _.asInstanceOf[ReadMode] }
  }

  private def addManualCal(): ISPSeqComponent =
    addSeqComponent(getObs.getSeqComponent, SPComponentType.OBSERVER_GEMFLAT)

  private def addAutomaticCal(): ISPSeqComponent =
    addSeqComponent(getObs.getSeqComponent, SeqRepeatSmartGcalObs.Arc.SP_TYPE)

  @Before
  override def setUp() {
    super.setUp()
    getObs.getSeqComponent.setSeqComponents(new util.ArrayList[ISPSeqComponent]())
  }

  @Test def testManualStepReadMode(): Unit = {
    val gnirs = getInstDataObj
    gnirs.setReadMode(VERY_FAINT)
    gnirs.setExposureTime(1.0)
    storeStaticUpdates()

    addManualCal()

    assertEquals(VERY_FAINT, readModeAt(0))
  }

  @Test def testAutomaticStepReadMode(): Unit = {
    val gnirs = getInstDataObj
    gnirs.setReadMode(VERY_FAINT)
    gnirs.setExposureTime(1.0)
    storeStaticUpdates()

    addAutomaticCal()

    assertEquals(BRIGHT, readModeAt(0))
  }

  @Test def testMixedStepReadMode(): Unit = {
    val gnirs = getInstDataObj
    gnirs.setReadMode(VERY_FAINT)
    gnirs.setExposureTime(1.0)
    storeStaticUpdates()

    addManualCal()
    addAutomaticCal()
    addManualCal()

    assertEquals(List(VERY_FAINT, BRIGHT, VERY_FAINT), readModes)
  }

}
