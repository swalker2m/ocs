// Copyright 1999 Association for Universities for Research in Astronomy, Inc.,
// Observatory Control System, Gemini Telescopes Project.
// See the file LICENSE for complete details.

package edu.gemini.pot.sp.memImpl;

import edu.gemini.pot.sp.*;
import edu.gemini.shared.util.GeminiRuntimeException;
import edu.gemini.spModel.core.SPBadIDException;
import edu.gemini.spModel.core.SPProgramID;
import edu.gemini.spModel.seqcomp.InstrumentSequenceSync;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * This object implements the ISPObservation and provides an in-memory,
 * non-persistent observation.
 */
public final class MemObservation extends MemAbstractContainer implements ISPObservation /*, Unreferenced*/ {
    private static final Logger LOG = Logger.getLogger(MemObservation.class.getName());

    public static final String OBS_QA_LOG_PROP = "ObsQaLog";
    public static final String OBS_EXEC_LOG_PROP = "ObsExecLog";

    private final MemProgram _program;
    private int _obsNumber;

    // The list of observation components
    private final List<ISPObsComponent> _compList = new ArrayList<ISPObsComponent>();

    private MemObsQaLog _obsQaLog;
    private MemObsExecLog _obsExecLog;

    // The root sequence component;
    private MemSeqComponent _sequenceComp;
    private MemSequence _sequence;

    MemObservation(MemProgram prog, int obsNumber, SPNodeKey key) throws SPException {
        super(prog.getDocumentData(), key);
        _program = prog;
        if (obsNumber < 0) {
            throw new IllegalArgumentException("obsNumber cannot be negative: " + obsNumber);
        }
        _obsNumber = obsNumber;
    }

    MemObservation(MemProgram prog, int obsNumber,
                   ISPFactory fact, ISPObservation obs, boolean preserveKeys) {
        super(prog.getDocumentData(), obs, preserveKeys);
        _program = prog;
        if (obsNumber < 0) {
            throw new IllegalArgumentException("obsNumber cannot be negative: " + obsNumber);
        }
        _obsNumber = obsNumber;

        // Copy the components.
        List<ISPObsComponent> compListCopy = obs.getObsComponents();
        ListIterator<ISPObsComponent> lit = compListCopy.listIterator();
        while (lit.hasNext()) {
            try {
                lit.set(fact.createObsComponentCopy(prog, lit.next(), preserveKeys));
            } catch (SPUnknownIDException ex) {
                // This should never happen since the factory handed in this
                // ProgramData
                throw new RuntimeException("Bug 1 in MemObservation copy constructor");
            }
        }

        final ISPObsQaLog qaLog = obs.getObsQaLog();
        final ISPObsExecLog execLog = obs.getObsExecLog();
        final ISPSeqComponent sc = obs.getSeqComponent();

        getProgramWriteLock(); // wouldn't strictly be required in this situation,
        // but updateChildren requires it

        try {
            ISPConflictFolder cf = obs.getConflictFolder();
            if (cf != null)
                setConflictFolder(fact.createConflictFolderCopy(prog, cf, preserveKeys));

            if (qaLog != null) {
                _obsQaLog = (MemObsQaLog) fact.createObsQaLogCopy(prog, qaLog, preserveKeys);
                _obsQaLog.attachTo(this);
            }

            if (execLog != null) {
                _obsExecLog = (MemObsExecLog) fact.createObsExecLogCopy(prog, execLog, preserveKeys);
                _obsExecLog.attachTo(this);
            }

            // Update the observation components.
            updateChildren(_compList, compListCopy);

            // Copy the sequence component and attach the copy.
            if (sc != null) {
                _sequenceComp = (MemSeqComponent) fact.createSeqComponentCopy(prog, sc, preserveKeys);
                _sequenceComp.attachTo(this);
            }
        } catch (SPException ex) {
            // This should never happen since we just created the components
            // locally and they have never been added to any observation yet.
            throw new RuntimeException("Bug 2 in MemObservation copy constructor");
        } finally {
            returnProgramWriteLock();
        }
    }

    public int getObservationNumber() {
        return _obsNumber;
    }

    public void setObservationNumber(int number) {
        _obsNumber = number;
    }

    public SPObservationID getObservationID() {
        SPProgramID progID = getDocumentData().getDocumentID();
        if (progID == null) return null;
        try {
            return new SPObservationID(progID, _obsNumber);
        } catch (SPBadIDException wontHappen) {
            throw GeminiRuntimeException.newException(wontHappen);
        }
    }

    public String getObservationIDAsString(String s) {
        SPObservationID spObsId = getObservationID();
        if (spObsId != null)
            return spObsId.toString();
        return s;
    }

    @Override
    public ISPObsQaLog getObsQaLog() {
        getProgramReadLock();
        try {
            return _obsQaLog;
        } finally {
            returnProgramReadLock();
        }
    }

    @Override
    public void setObsQaLog(ISPObsQaLog log) throws SPNodeNotLocalException, SPTreeStateException {
        if (log == _obsQaLog) return;
        final MemObsQaLog mol = (MemObsQaLog) log;
        getProgramWriteLock();
        try {
            final MemObsQaLog oldValue = _obsQaLog;
            if (log == null) {
                LOG.log(Level.FINE, "Deleting obs qa log of " + getObservationID(), new RuntimeException());
            } else if ((oldValue != null) && !oldValue.getNodeKey().equals(log.getNodeKey())) {
                LOG.log(Level.FINE, "Changing obs qa log of " + getObservationID(), new RuntimeException());
            }
            this._obsQaLog = mol;
            updateParentLinks(oldValue, mol);
            firePropertyChange(OBS_QA_LOG_PROP, oldValue, mol);
            fireStructureChange(OBS_QA_LOG_PROP, this, oldValue, mol);
        } finally {
            returnProgramWriteLock();
        }
    }

    @Override
    public ISPObsExecLog getObsExecLog() {
        getProgramReadLock();
        try {
            return _obsExecLog;
        } finally {
            returnProgramReadLock();
        }
    }

    @Override
    public void setObsExecLog(ISPObsExecLog log) throws SPNodeNotLocalException, SPTreeStateException {
        if (log == _obsExecLog) return;
        final MemObsExecLog mol = (MemObsExecLog) log;
        getProgramWriteLock();
        try {
            final MemObsExecLog oldValue = _obsExecLog;
            if (log == null) {
                LOG.log(Level.FINE, "Deleting obs exec log of " + getObservationID(), new RuntimeException());
            } else if ((oldValue != null) && !oldValue.getNodeKey().equals(log.getNodeKey())) {
                LOG.log(Level.FINE, "Changing obs exec log of " + getObservationID(), new RuntimeException());
            }
            this._obsExecLog = mol;
            updateParentLinks(oldValue, mol);
            firePropertyChange(OBS_EXEC_LOG_PROP, oldValue, mol);
            fireStructureChange(OBS_EXEC_LOG_PROP, this, oldValue, mol);
        } finally {
            returnProgramWriteLock();
        }
    }

    public List<ISPObsComponent> getObsComponents() {
        // Make a list copy so the client gets a stable version
        getProgramReadLock();
        try {
            return new ArrayList<ISPObsComponent>(_compList);
        } finally {
            returnProgramReadLock();
        }
    }

    public void setObsComponents(List<? extends ISPObsComponent> newCompList) throws SPNodeNotLocalException, SPTreeStateException {
        checkChildTypes(newCompList, ISPObsComponent.class);

        List<ISPObsComponent> newCopy = new ArrayList<ISPObsComponent>(newCompList);
        getProgramWriteLock();
        try {
            List<ISPObsComponent> oldCopy = new ArrayList<ISPObsComponent>(_compList);
            updateChildren(_compList, newCopy);
            firePropertyChange(OBS_COMPONENTS_PROP, oldCopy, newCopy);
            fireStructureChange(OBS_COMPONENTS_PROP, this, oldCopy, newCopy);
        } finally {
            returnProgramWriteLock();
        }
    }

    public void addObsComponent(ISPObsComponent obsComp) throws SPNodeNotLocalException, SPTreeStateException {
        // Get the local component (throwing an SPNodeNotLocalException if not
        // local).
        MemObsComponent node = (MemObsComponent) obsComp;
        getProgramWriteLock();
        try {
            List<ISPObsComponent> oldCopy = new ArrayList<ISPObsComponent>(_compList);
            node.attachTo(this);
            _compList.add(node);
            List<ISPObsComponent> newCopy = new ArrayList<ISPObsComponent>(_compList);
            firePropertyChange(OBS_COMPONENTS_PROP, oldCopy, newCopy);
            fireStructureChange(OBS_COMPONENTS_PROP, this, oldCopy, newCopy);
        } finally {
            returnProgramWriteLock();
        }
    }

    public void addObsComponent(int index, ISPObsComponent obsComp) throws IndexOutOfBoundsException, SPNodeNotLocalException, SPTreeStateException {
        // Get the local component (throwing an SPNodeNotLocalException if not
        // local).
        MemObsComponent node = (MemObsComponent) obsComp;
        getProgramWriteLock();
        try {
            List<ISPObsComponent> oldCopy = new ArrayList<ISPObsComponent>(_compList);
            node.attachTo(this);
            _compList.add(index, node);
            List<ISPObsComponent> newCopy = new ArrayList<ISPObsComponent>(_compList);
            firePropertyChange(OBS_COMPONENTS_PROP, oldCopy, newCopy);
            fireStructureChange(OBS_COMPONENTS_PROP, this, oldCopy, newCopy);
        } finally {
            returnProgramWriteLock();
        }
    }

    public void removeObsComponent(ISPObsComponent obsComp) {
        MemObsComponent node = (MemObsComponent) obsComp;
        getProgramWriteLock();
        try {
            int index = _compList.indexOf(node);
            if (index == -1) {
                System.out.println("Component was not located and can't be removed.");
                return;
            }
            List<ISPObsComponent> oldCopy = new ArrayList<ISPObsComponent>(_compList);
            node.detachFrom(this);
            _compList.remove(index);
            List<ISPObsComponent> newCopy = new ArrayList<ISPObsComponent>(_compList);
            firePropertyChange(OBS_COMPONENTS_PROP, oldCopy, newCopy);
            fireStructureChange(OBS_COMPONENTS_PROP, this, oldCopy, newCopy);
        } finally {
            returnProgramWriteLock();
        }
    }

    public ISPSeqComponent getSeqComponent() {
        return _sequenceComp;
    }

    public void setSeqComponent(ISPSeqComponent seqComp) throws SPNodeNotLocalException, SPTreeStateException {
        // Get the local component (throwing an SPNodeNotLocalException if not
        // local).
        MemSeqComponent node = (MemSeqComponent) seqComp;
        if (node == _sequenceComp) return;

        MemSeqComponent oldComp = _sequenceComp;
        getProgramWriteLock();
        try {
            updateParentLinks(oldComp, node);
            _sequenceComp = node;
            firePropertyChange(SEQ_COMPONENT_PROP, oldComp, node);
            fireStructureChange(SEQ_COMPONENT_PROP, this, oldComp, node);
        } finally {
            returnProgramWriteLock();
        }
    }

    public void removeSeqComponent() {
        getProgramWriteLock();
        try {
            MemSeqComponent oldValue = _sequenceComp;
            // It might be new and null
            if (_sequenceComp == null) {
                return;
            }
            _sequenceComp.detachFrom(this);
            _sequenceComp = null;
            firePropertyChange(SEQ_COMPONENT_PROP, oldValue, null);
            fireStructureChange(SEQ_COMPONENT_PROP, this, oldValue, null);
        } finally {
            returnProgramWriteLock();
        }
    }

    public ISPSequence getSequence() {
        return _sequence;
    }

    public void setSequence(ISPSequence sequence) throws SPNodeNotLocalException, SPTreeStateException {
        final MemSequence node = (MemSequence) sequence;
        if (node == _sequence) return;

        final MemSequence oldSequence = _sequence;
        getProgramWriteLock();
        try {
            updateParentLinks(oldSequence, node);
            _sequence = node;
            firePropertyChange(SEQUENCE_PROP, oldSequence, node);
            fireStructureChange(SEQUENCE_PROP, this, oldSequence, node);
        } finally {
            returnProgramWriteLock();
        }
    }

    public void removeSequence() {
        getProgramWriteLock();
        try {
            final MemSequence oldValue = _sequence;
            if (_sequence == null) return;

            _sequence.detachFrom(this);
            _sequence = null;
            firePropertyChange(SEQUENCE_PROP, oldValue, null);
            fireStructureChange(SEQUENCE_PROP, this, oldValue, null);
        } finally {
            returnProgramWriteLock();
        }
    }

    public void accept(ISPProgramVisitor visitor) {
        visitor.visitObservation(this);
    }

    public List<ISPNode> getChildren() {
        getProgramReadLock();
        try {
            List<ISPNode> res = new ArrayList<ISPNode>();
            if (getConflictFolder() != null) res.add(getConflictFolder());
            res.addAll(getObsComponents());
            if (_obsQaLog != null) res.add(_obsQaLog);
            if (_obsExecLog != null) res.add(_obsExecLog);
            ISPSeqComponent seqComp = getSeqComponent();
            if (seqComp != null) res.add(seqComp);

            final ISPSequence sequence = getSequence();
            if (sequence != null) res.add(sequence);
            return res;
        } finally {
            returnProgramReadLock();
        }
    }

    private static final Class[] VALID_CHILD_TYPES = {
            MemConflictFolder.class,
            MemObsComponent.class,
            MemObsQaLog.class,
            MemObsExecLog.class,
            MemSeqComponent.class,
            MemSequence.class,
    };

    protected void setTypedChildren(MemAbstractContainer.TypedChildren tc)
            throws SPNodeNotLocalException, SPTreeStateException {
        tc.verify(VALID_CHILD_TYPES);

        // OCSINF-264: we don't want the instrument component / sequence
        // synchronization to fire when we set the obs components.  In other
        // words, in the middle of updating the set of children.  Remove it
        // here and put it back once the obs components have been set.
        final Object obj = getClientData(InstrumentSequenceSync.USER_OBJ_KEY);
        removeClientData(InstrumentSequenceSync.USER_OBJ_KEY);

        setConflictFolder(tc.getOnlyChild(MemConflictFolder.class));
        setObsComponents(tc.getChildren(MemObsComponent.class));
        setObsQaLog(tc.getOnlyChild(MemObsQaLog.class));
        setObsExecLog(tc.getOnlyChild(MemObsExecLog.class));
        setSeqComponent(tc.getOnlyChild(MemSeqComponent.class));
        setSequence(tc.getOnlyChild(MemSequence.class));

        // Restore the instrument/sequence sync event monitor and guarantee
        // that the components being added are in sync. :-/
        putClientData(InstrumentSequenceSync.USER_OBJ_KEY, obj);
        InstrumentSequenceSync.syncFromInstrument(this);
    }

    @Override
    public MemProgram getProgram() {
        return _program;
    }

//    @Override
//    public int getStepCount() {
//        final ISPSeqComponent sc = getSeqComponent();
//        return (sc == null) ? 0 : sc.getStepCount();
//    }
}

