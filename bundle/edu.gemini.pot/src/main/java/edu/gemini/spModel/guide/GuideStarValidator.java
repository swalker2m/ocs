package edu.gemini.spModel.guide;

import edu.gemini.spModel.obs.context.ObsContext;
import edu.gemini.spModel.target.SPTarget;

/**
 *
 */
public interface GuideStarValidator {
    /**
     * Determines whether the given target is valid in the given context.
     * Drawing tools, for example, can use this information to indicate the
     * status of the guide star appropriately.
     *
     * @param guideStar guide star position to validate
     *
     * @param ctx context in which the guide star is validated
     *
     * @return a {@link edu.gemini.spModel.guide.GuideStarValidation}
     */
    GuideStarValidation validate(SPTarget guideStar, ObsContext ctx);
}
