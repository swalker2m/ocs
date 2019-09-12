package edu.gemini.ags.gems

import edu.gemini.catalog.api._
import edu.gemini.catalog.api.CatalogName.UCAC4
import edu.gemini.catalog.votable._
import edu.gemini.spModel.core.SiderealTarget
import edu.gemini.spModel.core.{Angle, MagnitudeBand, Coordinates}
import edu.gemini.spModel.gemini.gems.GemsInstrument
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.Conditions
import edu.gemini.spModel.obs.context.ObsContext

import scala.concurrent.{ExecutionContext, Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.collection.JavaConverters._
import scala.math._

import scalaz._
import Scalaz._

import jsky.util.gui.StatusLogger

/**
 * Implements GeMS guide star search. The catalog search will provide the inputs
 * to the analysis phase, which actually assigns guide stars to guiders.
 */
final case class GemsVoTableCatalog(
  catalog: CatalogName,
  backend: Option[VoTableBackend]
) {

  /**
   * Searches for the given base position according to the given options.
   * Multiple queries are performed in parallel in background threads.
   * This method is synchronous and can be used form the Java side of the OT
   *
   * @param obsContext   the context of the observation (needed to adjust for selected conditions)
   * @param basePosition the base position to search for
   * @param options      the search options
   * @param timeout      Timeout in seconds
   * @return list of search results
   */
  def search4Java(
    obsContext:   ObsContext,
    basePosition: Coordinates,
    options:      GemsGuideStarSearchOptions,
    timeout:      Int = 10,
    ec:           ExecutionContext
  ): java.util.List[SiderealTarget] =
    Await.result(search(obsContext, basePosition, options)(ec), timeout.seconds).asJava

  /**
   * Searches for the given base position according to the given options.
   * Multiple queries are performed in parallel in background threads.
   *
   * @param obsContext   the context of the observation (needed to adjust for selected conditions)
   * @param basePosition the base position to search for
   * @param options      the search options
   * @return  Future with a list of search results
   */
  def search(
    obsContext:   ObsContext,
    basePosition: Coordinates,
    options:      GemsGuideStarSearchOptions
  )(
    ec: ExecutionContext
  ): Future[List[SiderealTarget]] = {

    val gemsCriterion = options.canopusCriterion(obsContext)

    // TODO-NGS2: this assumes no offset positions. we need to do something like
    // TODO-NGS2: in RadiusLimitCalc (and see how to respect the min radius in
    // TODO-NGS2: Gaia searches since otherwise we may get way too many useless
    // TODO-NGS2: results)

    val queryArgs = CatalogQuery(
      basePosition,
      gemsCriterion.criterion.radiusConstraint,
      gemsCriterion.criterion.magConstraint,
      catalog
    )

    VoTableClient.catalog(queryArgs, backend)(ec).map(_.result.targets.rows)

  }

}
