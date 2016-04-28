package jsky.app.ot.ags;

import edu.gemini.pot.sp.SPNodeKey;

public interface BagsStatusListener {
    void bagsStatusChange(SPNodeKey key, BagsStatus oldStatus, BagsStatus newStatus);
}
