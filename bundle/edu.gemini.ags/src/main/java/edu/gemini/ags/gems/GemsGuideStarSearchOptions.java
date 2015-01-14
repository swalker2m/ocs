package edu.gemini.ags.gems;

import edu.gemini.catalog.api.MagnitudeConstraints;
import edu.gemini.skycalc.Angle;
import edu.gemini.shared.skyobject.Magnitude;
import edu.gemini.shared.util.immutable.None;
import edu.gemini.shared.util.immutable.Option;
import edu.gemini.spModel.gemini.gems.Canopus;
import edu.gemini.spModel.gemini.gems.GemsInstrument;
import edu.gemini.spModel.gems.GemsGuideProbeGroup;
import edu.gemini.spModel.gems.GemsGuideStarType;
import edu.gemini.spModel.gems.GemsTipTiltMode;
import edu.gemini.spModel.obs.context.ObsContext;

import java.util.*;

/**
 * An immutable class specifying the Gems guide star search options.
 * An instance of this class will be created by the UI or other client
 * and used to control the search process.
 *
 * See OT-25
 */
public class GemsGuideStarSearchOptions {

    public static enum CatalogChoice {
        PPMXL_CDS("PPMXL@CDS", "PPMXL at CDS"),
        PPMXL_CADC("PPMXL@CADC", "PPMXL at CADC"),
        UCAC3_CDS("UCAC3@CDS", "UCAC3 at CDS"),
        UCAC3_CADC("UCAC3@CADC", "UCAC3 at CADC"),
        NOMAD1_CDS("NOMAD1@CDS", "NOMAD1 at CDS"),
        NOMAD1_CADC("NOMAD1@CADC", "NOMAD1 at CADC"),
        USER_CATALOG("user", "User Catalog"),
        ;

        public static CatalogChoice DEFAULT = UCAC3_CDS;

        private String _displayValue;
        private String _catalogName;

        private CatalogChoice(String catalogName, String displayValue) {
            _displayValue = displayValue;
            _catalogName = catalogName;
        }

        public String displayValue() {
            return _displayValue;
        }

        public String catalogName() {
            return _catalogName;
        }

        public String toString() {
            return displayValue();
        }
    }


    public static enum NirBandChoice {
        J(Magnitude.Band.J),
        H(Magnitude.Band.H),
        K(Magnitude.Band.K),
        ;

        public static NirBandChoice DEFAULT = H;

        private Magnitude.Band _band;

        private NirBandChoice(Magnitude.Band band) {
            _band = band;
        }

        public Magnitude.Band getBand() {
            return _band;
        }

        public String displayValue() {
            return _band.name();
        }

        public String toString() {
            return displayValue();
        }
    }


    public static enum AnalyseChoice {
        BOTH("Canopus and GSAOI", GemsTipTiltMode.both),
        CANOPUS("Canopus", GemsTipTiltMode.canopus),
        GSAOI("GSAOI", GemsTipTiltMode.instrument),
        ;

//        public static AnalyseChoice DEFAULT = BOTH;
        public static AnalyseChoice DEFAULT = CANOPUS; // REL-604

        private String _displayValue;
        private GemsTipTiltMode _gemsTipTiltMode;

        private AnalyseChoice(String name, GemsTipTiltMode gemsTipTiltMode) {
            _displayValue = name;
            _gemsTipTiltMode = gemsTipTiltMode;
        }

        public String displayValue() {
            return _displayValue;
        }

        public String toString() {
            return displayValue();
        }

        public GemsTipTiltMode getGemsTipTiltMode() {
            return _gemsTipTiltMode;
        }
    }


    public static final String DEFAULT_CATALOG = CatalogChoice.DEFAULT.catalogName();

    private String opticalCatalog = DEFAULT_CATALOG;
    private String nirCatalog = DEFAULT_CATALOG;
    private GemsInstrument instrument;
    private GemsTipTiltMode tipTiltMode;
    private Magnitude.Band nirBand = NirBandChoice.DEFAULT.getBand();
    private Set<Angle> posAngles = new HashSet<>();


    public GemsGuideStarSearchOptions() {
    }

    public GemsGuideStarSearchOptions(final String opticalCatalog, final String nirCatalog, final GemsInstrument instrument,
                                      final GemsTipTiltMode tipTiltMode, final Set<Angle> posAngles) {
        this.opticalCatalog = opticalCatalog;
        this.nirCatalog = nirCatalog;
        this.instrument = instrument;
        if (instrument == GemsInstrument.flamingos2) {
            // Flamingos 2 OIWFS can only ever be used for the flexure star.
            this.tipTiltMode = GemsTipTiltMode.canopus;
        } else {
            this.tipTiltMode = tipTiltMode;
        }
        this.posAngles = posAngles;
    }

    public GemsInstrument getInstrument() {
        return instrument;
    }

    public GemsTipTiltMode getTipTiltMode() {
        return tipTiltMode;
    }

    public Set<Angle> getPosAngles() {
        return posAngles;
    }

    /**
     * @return a copy of this instance
     */
    public GemsGuideStarSearchOptions copy() {
        return new GemsGuideStarSearchOptions(opticalCatalog, nirCatalog, instrument,
                                      tipTiltMode, posAngles);
    }

    /**
     *
     * @param instrument
     * @return a copy of this instance with the given instrument
     */
    public GemsGuideStarSearchOptions setInstrument(final GemsInstrument instrument) {
        GemsGuideStarSearchOptions o = copy();
        o.instrument = instrument;
        return o;
    }

    /**
     *
     * @param posAngles
     * @return a copy of this instance with the given posAngles
     */
    public GemsGuideStarSearchOptions setPosAngles(final Set<Angle> posAngles) {
        GemsGuideStarSearchOptions o = copy();
        o.posAngles = posAngles;
        return o;
    }

    /**
     * @param nirBand      optional NIR magnitude band (default is H)
     * @return all relevant CatalogSearchCriterion instances
     */
    public List<GemsCatalogSearchCriterion> searchCriteria(final ObsContext obsContext, final Option<Magnitude.Band> nirBand) {
        switch(tipTiltMode) {
            case canopus: return Arrays.asList(
                    canopusCriterion(obsContext, GemsGuideStarType.tiptilt),
                    instrumentCriterion(obsContext, GemsGuideStarType.flexure, nirBand));
            case instrument: return Arrays.asList(
                    instrumentCriterion(obsContext, GemsGuideStarType.tiptilt, nirBand),
                    canopusCriterion(obsContext, GemsGuideStarType.flexure));
            default:
            case both: return Arrays.asList(
                    canopusCriterion(obsContext, GemsGuideStarType.tiptilt),
                    instrumentCriterion(obsContext, GemsGuideStarType.flexure, nirBand),
                    instrumentCriterion(obsContext, GemsGuideStarType.tiptilt, nirBand),
                    canopusCriterion(obsContext, GemsGuideStarType.flexure)
            );
        }
    }

    public GemsCatalogSearchCriterion canopusCriterion(final ObsContext obsContext, final GemsGuideStarType ggst) {
        final GemsMagnitudeTable.LimitsCalculator calculator = GemsMagnitudeTable.CanopusWfsMagnitudeLimitsCalculator();
        return searchCriterion(obsContext, Canopus.Wfs.Group.instance, calculator, ggst, None.<Magnitude.Band>instance());
    }

    public GemsCatalogSearchCriterion instrumentCriterion(final ObsContext obsContext, final GemsGuideStarType ggst, final Option<Magnitude.Band> nirBand) {
        final GemsMagnitudeTable.LimitsCalculator calculator = GemsMagnitudeTable.GemsInstrumentToMagnitudeLimitsCalculator().apply(instrument);
        return searchCriterion(obsContext, instrument.getGuiders(), calculator, ggst, nirBand);
    }

    public GemsCatalogSearchCriterion searchCriterion(final ObsContext obsContext,
                                                      final GemsGuideProbeGroup gGroup,
                                                      final GemsMagnitudeTable.LimitsCalculator calculator,
                                                      final GemsGuideStarType gType,
                                                      final Option<Magnitude.Band> nirBand) {
        final String name = String.format("%s %s", gGroup.getDisplayName(), gType.name());

        // Adjust the mag limits for the worst conditions (as is done in the ags servlet)
        final MagnitudeConstraints magConstraints = calculator.adjustGemsMagnitudeLimitsForJava(gType, nirBand, obsContext.getConditions());

        final CatalogSearchCriterion criterion = calculator.searchCriterionBuilder(name, gGroup.getRadiusLimits(), instrument, magConstraints, posAngles);
        final GemsCatalogSearchKey key = new GemsCatalogSearchKey(gType, gGroup);
        return new GemsCatalogSearchCriterion(key, criterion);
    }

    public Set<String> getCatalogs() {
        Set<String> catalogs = new HashSet<>(4);
        catalogs.add(nirCatalog);
        catalogs.add(opticalCatalog);
        return catalogs;
    }
}
