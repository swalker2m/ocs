package edu.gemini.pot.spdb;

import edu.gemini.pot.sp.ISPProgram;
import edu.gemini.pot.sp.SPCompositeChange;
import java.util.logging.Logger;
import java.util.logging.Level;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Handles trigger registration (and execution).
 */
final class TriggerRegistrar implements PropertyChangeListener, ProgramEventListener<ISPProgram> {
    private static final Logger LOG = Logger.getLogger(TriggerRegistrar.class.getName());

    private final ProgramManager<ISPProgram> _progMan;
    private final ExecutorService _pool;
    private Map<IDBTriggerCondition, List<TriggerReg>> _triggerMap = new HashMap<>();

    /**
     * Constructs with the program manager.
     */
    TriggerRegistrar(ProgramManager<ISPProgram> programMan) {
        _progMan = programMan;
        _pool = Executors.newCachedThreadPool();

        // Listen to all the programs.
        List<ISPProgram> progs = programMan.getPrograms();
        for (ISPProgram prog : progs) {
            prog.addCompositeChangeListener(this);
        }

        // Listen to the program manager to make sure we see any new programs.
        programMan.addListener(this);
    }


    public void register(IDBTriggerCondition condition, IDBTriggerAction action) {
        LOG.log(Level.INFO, "Registering trigger condition: " + condition);
        TriggerReg tr = new TriggerReg(condition, action);
        synchronized (this) {
            List<TriggerReg> actionList = _triggerMap.get(condition);
            if (actionList == null) {
                actionList = new ArrayList<>();
                _triggerMap.put(condition, actionList);
            }
            actionList.add(tr);
        }
    }

    public void unregister(IDBTriggerCondition condition, IDBTriggerAction action) {
        LOG.log(Level.INFO, "Unregistering trigger condition: " + condition);
        TriggerReg tr = new TriggerReg(condition, action);
        synchronized (this) {
            List<TriggerReg> actionList = _triggerMap.get(condition);
            if (actionList != null) {
                actionList.remove(tr);
                if (actionList.isEmpty())
                    _triggerMap.remove(condition);
            }
        }
    }


    private static class TriggerEvent {
        TriggerReg reg;
        Object handback;
        TriggerEvent(TriggerReg reg, Object handback) {
            this.reg = reg;
            this.handback = handback;
        }
    }

    /**
     * Extracts the TriggerReg objects that are interested in
     * handling the given composite change event.
     *
     * @return List of {@link TriggerEvent}
     */
    private synchronized List<TriggerEvent> _getMatchingRegs(SPCompositeChange change) {
        List<TriggerEvent> res = null;

        for (Map.Entry<IDBTriggerCondition, List<TriggerReg>> me : _triggerMap.entrySet()) {

            IDBTriggerCondition tc = me.getKey();
            Object handback = tc.matches(change);
            if (handback != null) {
                if (res == null) res = new ArrayList<>();
                List<TriggerReg> triggerRegList = me.getValue();
                for (TriggerReg reg : triggerRegList) {
                    res.add(new TriggerEvent(reg, handback));
                }
            }
        }

        return res;
    }

    /**
     * A Runnable used to execute a trigger action within a thread pool.
     */
    private static class TriggerTask implements Runnable {
        private final SPCompositeChange change;

        private final IDBTriggerAction action;
        private final Object handback;

        TriggerTask(SPCompositeChange change, TriggerEvent evt) {
            this.change = change;

            TriggerReg ltr = evt.reg;
            this.action = ltr.getTriggerAction();
            this.handback = evt.handback;
        }

        public void run() {

            // Record the start time
            long startTime = 0;
            if (action.getClass().getName().contains("TooAction")) {
                startTime = System.currentTimeMillis();
                LOG.log(Level.WARNING, "Sending a ToO alert...");
            }

            action.doTriggerAction(change, handback);

            // Record the end time and warn if it took too long.
            if (startTime != 0) {
                long curTime = System.currentTimeMillis();
                LOG.log(Level.WARNING, "Sent ToO alert");

                long elapsed = curTime - startTime;
                if (elapsed > 5000) {
                    LOG.log(Level.WARNING, "Long delay sending ToO alert: " + elapsed);
                }
            }

        }
    }

    /**
     * Fires a composite event to registered listeners.  Passes the event
     * along to any parent(s) to notify their composite listeners.
     */
    void handleEvent(SPCompositeChange change) {
        List<TriggerEvent> actionList = _getMatchingRegs(change);
        if (actionList == null) return;  // nobody cares

        // notify everyone
        for (TriggerEvent evt : actionList) {
            _pool.submit(new TriggerTask(change, evt));
        }
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        SPCompositeChange change = (SPCompositeChange) evt;
        handleEvent(change);
    }

    public void programAdded(ProgramEvent<ISPProgram> pme) {
        pme.getNewProgram().addCompositeChangeListener(TriggerRegistrar.this);
    }

    public void programReplaced(ProgramEvent<ISPProgram> pme) {
        programRemoved(pme);
        programAdded(pme);
    }

    public void programRemoved(ProgramEvent<ISPProgram> pme) {
        pme.getOldProgram().removeCompositeChangeListener(TriggerRegistrar.this);
    }

    /**
     * Cleans up.
     */
    void shutdown() {
        _pool.shutdownNow();
        _progMan.removeListener(this);

        for (ISPProgram o : _progMan.getPrograms())
            o.removeCompositeChangeListener(this);

    }
}
