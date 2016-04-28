package jsky.app.ot.ags

import jsky.app.ot.ags.BagsState.{IdleState, ErrorTransition, StateTransition}

import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import java.time.Instant
import java.util.concurrent.TimeoutException
import java.util.concurrent._
import java.util.logging.Logger

import edu.gemini.ags.api.{AgsHash, AgsRegistrar, AgsStrategy}
import edu.gemini.catalog.votable.{CatalogException, GenericError}
import edu.gemini.pot.sp._
import edu.gemini.spModel.core.SPProgramID
import edu.gemini.spModel.guide.GuideProbe
import edu.gemini.spModel.obs.{ObsClassService, ObservationStatus, SPObservation}
import edu.gemini.spModel.obs.context.ObsContext
import edu.gemini.spModel.obsclass.ObsClass
import edu.gemini.spModel.rich.shared.immutable._
import edu.gemini.spModel.target.env.{AutomaticGroup, GuideEnv, GuideEnvironment}
import jsky.app.ot.OT
import jsky.app.ot.gemini.altair.Altair_WFS_Feature
import jsky.app.ot.gemini.inst.OIWFS_Feature
import jsky.app.ot.gemini.tpe.TpePWFSFeature
import jsky.app.ot.tpe.{TpeContext, TpeManager}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext
import scala.swing.Swing
import scala.util.{Failure, Success, Try}
import scalaz._
import Scalaz._
import scalaz.effect.IO
import scalaz.effect.IO.ioUnit

sealed trait BagsState {
  def status: BagsStatus

  def edit: StateTransition =
    ErrorTransition

  def wakeUp: StateTransition =
    (this, ioUnit)

  def succeed(results: Option[AgsStrategy.Selection]): StateTransition =
    ErrorTransition

  def fail(why: String, delayMs: Long): StateTransition =
    ErrorTransition
}

object BagsState {
  type AgsHashVal      = Long
  type StateTransition = (BagsState, IO[Unit])
  val ErrorTransition  = (ErrorState, ioUnit)

  private def hashObs(obs: ISPObservation, ctx: ObsContext): AgsHashVal = {
    val when = obs.getDataObject.asInstanceOf[SPObservation].getSchedulingBlock.asScalaOpt.map(_.start) | Instant.now.toEpochMilli
    AgsHash.hash(ctx, when)
  }


  /** Error */
  case object ErrorState extends BagsState {
    override val status: BagsStatus = BagsStatus.Error
  }

  /** Idle */
  case class IdleState(obs: ISPObservation, hash: Option[AgsHashVal]) extends BagsState {
    override val status: BagsStatus = BagsStatus.Idle

    override val edit: StateTransition =
      (PendingState(obs, hash), BagsManager.wakeUpAction(obs, 0))
  }

  /** Pending */
  case class PendingState(obs: ISPObservation, hash: Option[AgsHashVal]) extends BagsState {
    override val status: BagsStatus = BagsStatus.Pending

    override val edit: StateTransition =
      (this, ioUnit)

    override def wakeUp: StateTransition = {
      def notObserved: Boolean =
        ObservationStatus.computeFor(obs) != ObservationStatus.OBSERVED

      val tup = for {
        c <- ObsContext.create(obs).asScalaOpt
        s <- BagsManager.agsStrategy(obs, c)
      } yield (c, s)

      val newHash = tup.map { case (ctx, _) => hashObs(obs, ctx) }

      val runningTransition = for {
        (c, s) <- tup
        h      <- newHash
        if !hash.contains(h) && notObserved
      } yield (RunningState(obs, h), BagsManager.triggerAgsAction(obs)): StateTransition

      def idleAction = tup.fold(BagsManager.clearAction(obs)) { _ => ioUnit }

      runningTransition | ((IdleState(obs, newHash), idleAction))
    }
  }

  /** Running */
  case class RunningState(obs: ISPObservation, hash: AgsHashVal) extends BagsState {
    override val status: BagsStatus = BagsStatus.Running

    override val edit: StateTransition =
      (RunningEditedState(obs, hash), ioUnit)

    override def succeed(results: Option[AgsStrategy.Selection]): StateTransition =
      (IdleState(obs, Some(hash)), BagsManager.applyAction(obs, results))

    override def fail(why: String, delayMs: Long): StateTransition =
      (FailureState(obs, why), BagsManager.wakeUpAction(obs, delayMs))
  }

  /** RunningEdited */
  case class RunningEditedState(obs: ISPObservation, hash: AgsHashVal) extends BagsState {
    override val status: BagsStatus = BagsStatus.Running

    override val edit: StateTransition =
      (this, ioUnit)

    override def succeed(results: Option[AgsStrategy.Selection]): StateTransition = {
      val action = for {
        _ <- BagsManager.applyAction(obs, results)
        _ <- BagsManager.wakeUpAction(obs, 0)
      } yield ()
      (PendingState(obs, Some(hash)), action)
    }

    override def fail(why: String, delayMs: Long): StateTransition =
      (PendingState(obs, None), BagsManager.wakeUpAction(obs, delayMs))
  }

  /** Failure */
  case class FailureState(obs: ISPObservation, why: String) extends BagsState {
    override def status: BagsStatus = BagsStatus.Failed(why)

    override val edit: StateTransition =
      (this, ioUnit)

    override val wakeUp: StateTransition =
      (PendingState(obs, None), BagsManager.wakeUpAction(obs, 0))
  }
}


object BagsManager {
  private val Log = Logger.getLogger(getClass.getName)

  val NumWorkers = math.max(1, Runtime.getRuntime.availableProcessors - 1)
  private val worker = new ScheduledThreadPoolExecutor(NumWorkers, new ThreadFactory() {
    override def newThread(r: Runnable): Thread = {
      val t = new Thread(r, "BagsManager - Worker")
      t.setPriority(Thread.NORM_PRIORITY - 1)
      t.setDaemon(true)
      t
    }
  })

  private implicit val executionContext = ExecutionContext.fromExecutor(worker)

  private implicit val OrderSPProgramID: Order[SPProgramID] =
    Order.order((p0, p1) => Ordering.fromInt(p0.compareTo(p1)))

  private implicit val OrderSPNodeKey: Order[SPNodeKey] =
    Order.order((k0, k1) => Ordering.fromInt(k0.compareTo(k1)))

  private var stateMap = ==>>.empty[SPProgramID, SPNodeKey ==>> BagsState]

  private def transition(obs: ISPObservation, update: BagsState => StateTransition): Unit = Swing.onEDT {
    val key = obs.getNodeKey

    val stateTuple = for {
      pid    <- Option(obs.getProgramID)
      obsMap <- stateMap.lookup(pid)
      state  <- obsMap.lookup(key)
    } yield (pid, obsMap, state)

    stateTuple.foreach { case (pid, obsMap, state) =>
      val (newState, sideEffect) = update(state)
      val newObsMap              = obsMap.insert(key, newState)
      val newStatemap            = stateMap.insert(pid, newObsMap)

      val oldStatus = state.status
      val newStatus = newState.status

      val action = for {
        _ <- IO { stateMap = newStatemap }
        _ <- sideEffect
        _ <- (oldStatus == newStatus) ? ioUnit | IO { notify(key, oldStatus, newStatus) }
      } yield ()

      action.unsafePerformIO()
    }
  }

  private def edited(obs: ISPObservation): Unit =
    transition(obs, _.edit)

  private def wakeUp(obs: ISPObservation): Unit =
    transition(obs, _.wakeUp)

  private def success(obs: ISPObservation, results: Option[AgsStrategy.Selection]): Unit =
    transition(obs, _.succeed(results))

  private def fail(obs: ISPObservation, why: String, delay: Long): Unit =
    transition(obs, _.fail(why, delay))

  def bagsStatus(o: ISPObservation): Option[BagsStatus] =
    for {
      pid <- Option(o.getProgram).map(_.getProgramID)
      s   <- bagsStatus(pid, o.getNodeKey)
    } yield s

  def bagsStatus(pid: SPProgramID, key: SPNodeKey): Option[BagsStatus] = {
    for {
      m <- stateMap.lookup(pid)
      s <- m.lookup(key)
    } yield s.status
  }

  /**
    * Atomically add a program to our watch list and attach listeners.
    * Enqueue all observations for consideration for a BAGS lookup.
    */
  def watch(prog: ISPProgram): Unit = {
    prog.addCompositeChangeListener(CompositePropertyChangeListener)

    val obsList = prog.getAllObservations.asScala.toList
    val obsMap  = ==>>.fromList(obsList.map(o => o.getNodeKey -> (IdleState(o, None): BagsState)))

    Option(prog.getProgramID).foreach { pid =>
      stateMap = stateMap + (pid -> obsMap)
    }

    obsList.foreach(edited)
  }

  /**
    * Atomically remove a program from our watch list and remove listeners. Any queued observations
    * will be discarded as they come up for consideration.
    */
  def unwatch(prog: ISPProgram): Unit = {
    prog.removeCompositeChangeListener(CompositePropertyChangeListener)

    Option(prog.getProgramID).foreach { pid =>
      stateMap = stateMap - pid
    }
  }

  private def enqueue(o: ISPObservation): Unit =  {
    Option(o.getProgramID).foreach { pid =>
      val obsMap  = stateMap.lookup(pid).getOrElse(==>>.empty[SPNodeKey, BagsState])
      val obsMap2 = obsMap.alter(o.getNodeKey, {
        case None => Some(IdleState(o, None))
        case x    => x
      })
      stateMap = stateMap + (pid -> obsMap2)
    }
    BagsManager.edited(o)
  }

  object CompositePropertyChangeListener extends PropertyChangeListener {
    override def propertyChange(evt: PropertyChangeEvent): Unit =
      evt.getSource match {
        case node: ISPNode => Option(node.getContextObservation).foreach(enqueue)
        case _             => // Ignore
      }
  }

  /** Listeners for BAGS status changes. **/
  private var listeners: Set[BagsStatusListener] = Set.empty[BagsStatusListener]

  def addBagsStatusListener(l: BagsStatusListener): Unit = {
    listeners = listeners + l
  }

  def removeBagsStatusListener(l: BagsStatusListener): Unit = {
    listeners = listeners - l
  }

  def clearBagsStateListeners(): Unit = {
    listeners = Set.empty
  }

  private def notify(key: SPNodeKey, oldStatus: BagsStatus, newStatus: BagsStatus): Unit =
    listeners.foreach(l => Try { l.bagsStatusChange(key, oldStatus, newStatus) })


  // Performs checks to rule out disabled guide groups, instruments without guiding strategies (e.g. GPI),
  // observation classes that do not have guiding (e.g. daytime calibration).
  def agsStrategy(obs: ISPObservation, ctx: ObsContext): Option[AgsStrategy] = {
    val grouEnabledp = ctx.getTargets.getGuideEnvironment.guideEnv.auto match {
      case AutomaticGroup.Initial | AutomaticGroup.Active(_, _) => true
      case _                                                    => false
    }

    AgsRegistrar.currentStrategy(ctx).filter { _ =>
      grouEnabledp && (ObsClassService.lookupObsClass(obs) != ObsClass.DAY_CAL)
    }
  }

  private[ags] def wakeUpAction(obs: ISPObservation, delayMs: Long): IO[Unit] = IO {
    worker.schedule(new Runnable() {
      def run(): Unit = BagsManager.wakeUp(obs)
    }, delayMs, TimeUnit.MILLISECONDS)
  }

  private[ags] def triggerAgsAction(obs: ISPObservation): IO[Unit] = IO {
    val bagsIdMsg = s"BAGS lookup on thread=${Thread.currentThread.getId} for observation=${obs.getObservationID}"
    Log.info(s"Performing $bagsIdMsg.")

    val fut = for {
      ctx <- ObsContext.create(obs).asScalaOpt
      ags <- AgsRegistrar.currentStrategy(ctx)
    } yield ags.select(ctx, OT.getMagnitudeTable)

    fut.fold(BagsManager.fail(obs, "Cannot perform AGS on this observation", 0)) { f =>
      f.onComplete {
        case Success(opt) =>
          Thread.sleep(2000)
          BagsManager.success(obs, opt)

        case Failure(CatalogException((e: GenericError) :: _)) =>
          Thread.sleep(2000)
          BagsManager.fail(obs, "Catalog lookup failed.", 5000)

        case Failure(ex: TimeoutException) =>
          Thread.sleep(2000)
          BagsManager.fail(obs, "Catalog timed out.", 0)

        case Failure(ex) =>
          Thread.sleep(2000)
          BagsManager.fail(obs, s"Unexpected error ${Option(ex.getMessage).getOrElse("")}", 5000)
      }
    }
  }

  private[ags] def clearAction(obs: ISPObservation): IO[Unit] =
    applyAction(obs, None)

  private[ags] def applyAction(obs: ISPObservation, selOpt: Option[AgsStrategy.Selection]): IO[Unit] = IO {
    def applySelection(ctx: TpeContext): Unit = {
      val oldEnv = ctx.targets.envOrDefault

      // If AGS results were found, apply them to the target env; otherwise, clear out any existing auto group.
      val newEnv = selOpt.map(_.applyTo(oldEnv)).getOrElse {
        val oldGuideEnv = oldEnv.getGuideEnvironment.guideEnv
        if (oldGuideEnv.auto === AutomaticGroup.Initial || oldGuideEnv.auto === AutomaticGroup.Disabled) oldEnv
        else oldEnv.setGuideEnvironment(GuideEnvironment(GuideEnv(AutomaticGroup.Initial, oldGuideEnv.manual)))
      }

      // Calculate the new target environment and if they are different referentially, apply them.
      ctx.targets.dataObject.foreach { targetComp =>
        // If the env reference hasn't changed, this does nothing.
        if (oldEnv != newEnv) {
          targetComp.setTargetEnvironment(newEnv)
          ctx.targets.commit()
        }

        // Change the pos angle as appropriate if this is the auto group.
        selOpt.foreach { sel =>
          if (newEnv.getPrimaryGuideGroup.isAutomatic && selOpt.isDefined) {
            ctx.instrument.dataObject.foreach { inst =>
              val deg = sel.posAngle.toDegrees
              val old = inst.getPosAngleDegrees
              if (deg != old) {
                inst.setPosAngleDegrees(deg)
                ctx.instrument.commit()
              }
            }
          }
        }
      }
    }

    obs.getProgram.removeCompositeChangeListener(CompositePropertyChangeListener)
    applySelection(TpeContext(obs))

    // Update the TPE if it is visible
    selOpt.foreach { sel =>
      Option(TpeManager.get()).filter(_.isVisible).foreach { tpe =>
        sel.assignments.foreach { ass =>
          val clazz = ass.guideProbe.getType match {
            case GuideProbe.Type.AOWFS => classOf[Altair_WFS_Feature]
            case GuideProbe.Type.OIWFS => classOf[OIWFS_Feature]
            case GuideProbe.Type.PWFS  => classOf[TpePWFSFeature]
          }
          Option(tpe.getFeature(clazz)).foreach(tpe.selectFeature)
        }
      }
    }

    obs.getProgram.addCompositeChangeListener(CompositePropertyChangeListener)
  }
}