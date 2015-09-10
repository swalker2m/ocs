//
// $Id$
//

package edu.gemini.spModel.dataflow;

import edu.gemini.shared.util.immutable.Option;
import edu.gemini.spModel.core.ProgramType$;
import edu.gemini.spModel.dataset.DatasetLabel;
import edu.gemini.spModel.pio.ParamSet;
import edu.gemini.spModel.pio.Pio;
import edu.gemini.spModel.pio.PioFactory;
import edu.gemini.spModel.core.ProgramType;

import java.util.Map;
import java.util.HashMap;
import java.io.Serializable;

/**
 * Contains Gemini Science Archive related attributes.
 */
public class GsaAspect implements Serializable {
    private static final String SEND_TO_GSA         = "sendToGsa";
    private static final String PROPRIETARY_MONTHS  = "proprietaryMonths";
    private static final String KEEP_HEADER_PRIVATE = "headerPrivate";

    public static final GsaAspect DEFAULT = new GsaAspect(false, 0);

    private static final Map<ProgramType, GsaAspect> TYPE_MAP = new HashMap<ProgramType, GsaAspect>();

    static {
        TYPE_MAP.put(ProgramType.Calibration$.MODULE$,        new GsaAspect(true,  0));
        TYPE_MAP.put(ProgramType.Classical$.MODULE$,          new GsaAspect(true, 18));
        TYPE_MAP.put(ProgramType.DirectorsTime$.MODULE$,      new GsaAspect(true, 18));
        TYPE_MAP.put(ProgramType.FastTurnaround$.MODULE$,     new GsaAspect(true,  6));
        TYPE_MAP.put(ProgramType.LargeProgram$.MODULE$,       new GsaAspect(true, 18));
        TYPE_MAP.put(ProgramType.Queue$.MODULE$,              new GsaAspect(true, 18));
        TYPE_MAP.put(ProgramType.SystemVerification$.MODULE$, new GsaAspect(true,  3));
    }

    public static GsaAspect getDefaultAspect(Option<ProgramType> type) {
        return getDefaultAspect(type.getOrNull());
    }

    public static GsaAspect getDefaultAspect(ProgramType type) {
        if (type == null) return DEFAULT;
        GsaAspect res = TYPE_MAP.get(type);
        return (res == null) ? DEFAULT : res;
    }

    public static GsaAspect getDefaultAspect(DatasetLabel label) {
        return getDefaultAspect(ProgramType$.MODULE$.readOrNull(label.getProgramId()));
    }

    public static boolean isSendToGsa(DatasetLabel label) {
        return label != null && getDefaultAspect(label).isSendToGsa();
    }

    private boolean _sendToGsa;
    private int _proprietaryMonths = -1;
    private boolean _keepHeaderPrivate;

    public GsaAspect(boolean sendToGsa, int months) {
        this(sendToGsa, months, false);
    }

    public GsaAspect(boolean sendToGsa, int months, boolean headerPrivate) {
        _sendToGsa         = sendToGsa;
        _proprietaryMonths = months;
        _keepHeaderPrivate = headerPrivate;
    }

    /**
     * Set the state of this object from the given parameter set.
     */
    public GsaAspect(ParamSet paramSet) {
        if (paramSet == null) return;
        _sendToGsa         = Pio.getBooleanValue(paramSet, SEND_TO_GSA, false);
        _proprietaryMonths = Pio.getIntValue(paramSet, PROPRIETARY_MONTHS, -1);
        _keepHeaderPrivate = Pio.getBooleanValue(paramSet, KEEP_HEADER_PRIVATE, false);
    }

    public boolean isSendToGsa() {
        return _sendToGsa;
    }

    public int getProprietaryMonths() {
        return _proprietaryMonths;
    }

    public boolean isHeaderPrivate() {
        return _keepHeaderPrivate;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        GsaAspect that = (GsaAspect) o;

        if (_sendToGsa != that._sendToGsa) return false;
        if (_keepHeaderPrivate != that._keepHeaderPrivate) return false;
        return (_proprietaryMonths == that._proprietaryMonths);
    }

    public int hashCode() {
        int result = _proprietaryMonths;
        result = 31 * result + (_keepHeaderPrivate ? 1 : 0);
        result = 31 * result + (_sendToGsa ? 1 : 0);
        return result;
    }

    /**
     * Return a parameter set describing the current state of this object.
     */
    public ParamSet getParamSet(PioFactory factory, String name) {
        ParamSet paramSet = factory.createParamSet(name);
        Pio.addBooleanParam(factory, paramSet, SEND_TO_GSA, _sendToGsa);
        Pio.addIntParam(factory, paramSet, PROPRIETARY_MONTHS, _proprietaryMonths);
        Pio.addBooleanParam(factory, paramSet, KEEP_HEADER_PRIVATE, _keepHeaderPrivate);
        return paramSet;
    }
}
