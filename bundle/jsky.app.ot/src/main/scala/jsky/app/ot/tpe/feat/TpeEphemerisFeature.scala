package jsky.app.ot.tpe.feat

import java.awt.font.TextLayout
import java.awt.geom.{Line2D, GeneralPath}
import java.awt.geom.Point2D.{ Double => Point }
import java.awt.{ Graphics2D, Color, Graphics}
import java.awt.RenderingHints._
import java.text.SimpleDateFormat
import java.util.{ Date, TimeZone }

import edu.gemini.pot.sp.ISPObservation
import edu.gemini.spModel.core._
import edu.gemini.spModel.obs.plannedtime.PlannedTimeCalculator
import jsky.app.ot.tpe.{TpeContext, TpeImageFeature, TpeImageFeatureCategory, TpeImageInfo}
import jsky.coords.CoordinateConverter.{ WORLD, SCREEN }

import scala.collection.JavaConverters._
import scalaz._, Scalaz._

class TpeEphemerisFeature extends TpeImageFeature("Ephemeris", "Show interpolated ephemeris.") {
  import TpeEphemerisFeature._

  override val getCategory = TpeImageFeatureCategory.target
  override val isEnabledByDefault = true
  override def isEnabled(ctx: TpeContext) = ctx.targets.base.exists(_.getNonSiderealTarget.isDefined)

  def toScreenCoordinates(c: Coordinates): Option[Point] =
    scala.util.Try {
      val p = new Point(c.ra.toDegrees, c.dec.toDegrees)
      _iw.plotter.getCoordinateConverter.convertCoords(p, WORLD, SCREEN, false)
      p
    }.toOption

  def toScreenEphemeris(e: Ephemeris): ScreenEphemeris =
    e.toList.flatMap { case (t, c) => toScreenCoordinates(c).strengthL(t) }

  def getEphemeris: Ephemeris =
    getContext.targets.base
      .flatMap(_.getNonSiderealTarget)
      .fold(Ephemeris.empty)(_.ephemeris)

  def getScreenEphemeris: ScreenEphemeris =
    toScreenEphemeris(getEphemeris)

  def obsTime: Option[Long] =
    getContext.schedulingBlock.flatMap(_.duration) orElse
    getContext.obsShell.map(remainingTime)

  def obsScreenEphemeris: Option[ScreenEphemeris] =
    for {
      lo <- getContext.schedulingBlockStart
      hi <- obsTime.map(_ + lo)
      e  <- getEphemeris.iSlice(lo, hi)
    } yield toScreenEphemeris(e)

  def draw(g: Graphics, tii: TpeImageInfo): Unit =
    draw2D(g.asInstanceOf[Graphics2D])

  def draw2D(g: Graphics2D): Unit = {

    // This seems to help
    g.setRenderingHint(KEY_ANTIALIASING,      VALUE_ANTIALIAS_ON)
    g.setRenderingHint(KEY_RENDERING,         VALUE_RENDER_QUALITY)
    g.setRenderingHint(KEY_TEXT_ANTIALIASING, VALUE_TEXT_ANTIALIAS_ON)

    // Save these
    val origColor = g.getColor
    val origFont  = g.getFont

    g.setColor(Color.RED)
    g.setFont(TpeImageFeature.FONT)

    // The main ephemeris track
    val se = getScreenEphemeris
    drawTrack (g, se)
    drawLabels(g, se)

    // Highlighted track for remainder of observation, and label at the last point
    // on the highlighted track.
    obsScreenEphemeris.foreach { se =>
      g.setColor(Color.GREEN)
      drawTrack(g, se)
      se.takeRight(2) match {
        case List((_, p1), (t, p2)) => drawLabel(g, t, p2, ang(p1, p2))
        case _ => // degenerate ephemeris; no track was drawn, ignore
      }
    }

    // Restore old values
    g.setColor(origColor)
    g.setFont(origFont)
    
  }

  def drawTrack(g: Graphics2D, e: ScreenEphemeris): Unit =
    g.draw(e.map(_._2).toGeneralPath)

  def drawLabels(g: Graphics2D, e: ScreenEphemeris): Unit = {

    // Screen ephemeris with angles.
    val sea: List[(Long, Point, Double)] =
      e.sliding(3).collect {
        case List((_, p1), (t2, p2), (_, p3)) => (t2, p2, ang(p1, p3))
      } .toList

    // Drop elements to guarantee that adjacent points are at least `PixelDistance` apart.
    val sparse: List[(Long, Point, Double)] =
      sparsify(sea) { case ((_, p1, _), (_, p2, _)) =>
        p1.distance(p2.x, p2.y) < PixelDistance
      }

    // Draw a tick mark and label at each remaining point.
    sparse.foreach { case (t, point, angle) =>
      drawLabel(g, t, point, angle)
    }

  }

  def drawLabel(g: Graphics2D, t: Long, point: Point, angle: Double): Unit = {
    val layout = new TextLayout(formatDate(t), g.getFont, g.getFontRenderContext)
    g.translate(point.x,  point.y)
    g.rotate(angle)
    g.draw(new Line2D.Double(-TickSize, 0.0, TickSize, 0.0))
    g.translate(TickSize * 2, TickSize)
    layout.draw(g, 0, 0)
    g.translate(-TickSize * 2, -TickSize)
    g.rotate(- angle)
    g.translate(-point.x, -point.y)
  }

}

object TpeEphemerisFeature {

  type ScreenEphemeris = List[(Long, Point)]

  val TickSize      = 3.0   // size of tick marks in pixels
  val PixelDistance = 100.0 // min distance between labeled points
  
  // angle of line from p1 to p2 in screen coordinates
  def ang(p1: Point, p2: Point): Double =
    if ((p1.x - p2.x).abs < 0.1) 0.0 // ~inf = 0
    else {
      val slope = (p2.y - p1.y) / (p2.x - p1.x)
      val angle = math.atan(slope) - Math.PI / 2.0
      if (slope < 0.0) angle - Math.PI else angle
    }

  // discard adjacent elements that meet some predicate
  def sparsify[A](as: List[A])(f: (A, A) => Boolean): List[A] = {
    @annotation.tailrec def go(as: List[A], last: A, acc: List[A]): List[A] =
      as match {
        case Nil => acc.reverse
        case h :: tail =>
          if (f(last, h)) go(tail, last, acc)
          else go(tail, h, h :: acc)
      }
    as match {
      case Nil => Nil
      case a :: as => go(as, a, Nil)
    }
  }

  val formatDate: Long => String = {
    val df = new SimpleDateFormat("dd-MMM HH:mm")
    df.setTimeZone(TimeZone.getTimeZone("UTC"))
    t => df.synchronized(df.format(new Date(t)))
  }

  implicit class ListOfPoint2DOps(ps: List[Point]) {
    def toGeneralPath: GeneralPath = {
      val path = new GeneralPath
      ps match {
        case Nil     =>
        case p :: ps =>
          path.moveTo(p.x, p.y)
          ps.foreach(p => path.lineTo(p.x, p.y))
      }
      path
    }
  }

  def remainingTime(ispObservation: ISPObservation): Long =
    PlannedTimeCalculator.instance
      .calc(ispObservation)
      .steps.asScala
      .filterNot(_.executed)
      .map(_.totalTime)
      .sum

}