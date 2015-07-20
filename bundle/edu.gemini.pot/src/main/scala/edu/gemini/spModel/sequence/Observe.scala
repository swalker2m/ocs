package edu.gemini.spModel.sequence

import edu.gemini.spModel.obsclass.ObsClass

/**
 * oType   => observe:observeType
 * oObject => observe:object
 * oClass  => observe:class
 */
final case class Observe(oType: String, oObject: String, oClass: ObsClass)
