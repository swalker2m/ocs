package jsky.image.gui;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.xy.XYDataset;
import org.jfree.data.general.DatasetChangeListener;
import org.jfree.data.general.DatasetGroup;
import org.jfree.data.DomainOrder;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.geom.Rectangle2D;
import java.awt.image.DataBuffer;

import javax.media.jai.Histogram;
import javax.media.jai.ROI;
import javax.media.jai.ROIShape;
import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;

import jsky.image.BasicImageReadableProcessor;
import jsky.image.ImageChangeEvent;
import jsky.image.ImageProcessor;
import jsky.util.I18N;
import jsky.util.gui.VRangeSlider;

/**
 * Dialog to view and edit the image cut levels.
 *
 * @version $Revision: 40560 $
 * @author Allan Brighton
 */
public class ImageCutLevels extends JPanel {

    // Used to access internationalized strings (see i18n/gui*.proprties)
    private static final I18N _I18N = I18N.getInstance(ImageCutLevels.class);

    // The top level parent frame (or internal frame) used to close the window
    protected Component parent;

    /** Used to plot a histogram of image pixel value distribution. */
    protected JFreeChart chart;

    /** Object managing image processing operations (including setting cut levels) */
    protected ImageProcessor imageProcessor;

    /** main image display window (to get visible area) */
    protected BasicImageReadableProcessor imageDisplay;

    /** widget for editing the cut levels */
    protected VRangeSlider rangeSlider;

    /** current low cut value */
    protected double lowCut = 0.;

    /** current high cut value */
    protected double highCut = 0.;

    /** The number of values in the plot */
    protected int numValues = 0;

    /** True if change events for the slider should be ignored */
    protected boolean ignoreChangeEvents = false;

    /** default number of bins in histogram (may be less) */
    protected static final int HISTOGRAM_SIZE = 2048;

    /**
     * Constructor
     */
    public ImageCutLevels(final Component parent, BasicImageReadableProcessor imageDisplay) {
        this.parent = parent;
        this.imageDisplay = imageDisplay;
        imageProcessor = imageDisplay.getImageProcessor();

        setLayout(new BorderLayout());
        add(makeGraph(), BorderLayout.CENTER);

        JPanel bot = new JPanel();
        bot.setLayout(new BorderLayout());
        add(bot, BorderLayout.SOUTH);

        bot.add(makeControlPanel(), BorderLayout.NORTH);
        bot.add(makeButtonPanel(), BorderLayout.SOUTH);

        imageProcessor.addChangeListener(ce -> {
            ImageChangeEvent e = (ImageChangeEvent) ce;
            if (parent.isVisible() && e.isNewCutLevels()) {
                updateDisplay();
            }
        });

        // initialize the display
        updateDisplay();
    }

    /**
     * Make a graph window to chart the image pixel distribution
     */
    ChartPanel makeGraph() {
        String title = _I18N.getString("pixelValueDist");
        String xAxisLabel = _I18N.getString("pixelValue");
        String yAxisLabel = _I18N.getString("frequency");
        XYDataset data = new SimpleDataset();
        chart = ChartFactory.createXYLineChart(title, xAxisLabel, yAxisLabel, data,
                PlotOrientation.VERTICAL, false, false, false);
        ChartPanel chartPanel = new ChartPanel(chart);
        chartPanel.setPreferredSize(new Dimension(250, 250));
        return chartPanel;
    }


    /**
     * Plot a histogram for the image
     */
    public void plotHistogram() {
        numValues = HISTOGRAM_SIZE;
        int dataType = imageProcessor.getRescaledSourceImage().getSampleModel().getDataType();
        boolean isFloatingPoint = (dataType == DataBuffer.TYPE_FLOAT || dataType == DataBuffer.TYPE_DOUBLE);
        double n = highCut - lowCut;

        if (n < numValues && !isFloatingPoint)
            numValues = (int) n;

        if (numValues <= 0) {
            chart.getXYPlot().setDataset(new SimpleDataset());
            return;
        }

        double[] xValues = new double[numValues];
        int[] yValues = new int[numValues];
        double m = lowCut;
        double factor = n / numValues;

        // the X values are the pixel values
        // the Y values are the number of pixels in a given range
        for (int i = 0; i < numValues; i++, m += factor) {
            xValues[i] = m;
            yValues[i] = 0;
        }
        if (factor >= 0.0) {
            Rectangle2D.Double region = imageDisplay.getVisibleArea();
            ROI roi = new ROIShape(region);
            Histogram histogram = imageProcessor.getHistogram(numValues, roi);
            yValues = histogram.getBins(0);
            chart.getXYPlot().setDataset(new SimpleDataset(xValues, yValues));
        }
    }

    /**
     * Make a control panel for setting the cut levels
     */
    JPanel makeControlPanel() {
        JPanel panel = new JPanel();

        panel.setLayout(new BorderLayout());
        panel.setBorder(BorderFactory.createEtchedBorder());

        panel.add(makeSliderPanel(), BorderLayout.NORTH);
        panel.add(makePercentPanel(), BorderLayout.SOUTH);

        return panel;
    }


    /**
     * Make a panel with a slider for adjusting the cut levels.
     */
    JPanel makeSliderPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout());

        rangeSlider = new VRangeSlider(_I18N.getString("cutLevels"), imageProcessor.getMinValue(), imageProcessor.getMaxValue());
        rangeSlider.setValues(imageProcessor.getLowCut(), imageProcessor.getHighCut());
        rangeSlider.addChangeListener(ce -> {
            if (ignoreChangeEvents)
                return;
            setCutLevels(rangeSlider.getMinValue(), rangeSlider.getMaxValue());
        });

        panel.add(rangeSlider, BorderLayout.NORTH);
        return panel;
    }


    /**
     * Make a panel with convenience buttons for setting cut levels.
     */
    JPanel makePercentPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

        JLabel label = new JLabel(_I18N.getString("autoSet") + ":");
        panel.add(label);

        // XXX fix number format for different locales...
        String[] ar = {"90.0", "95.0", "98.0", "99.0", "99.5", "100.0"};
        for (String anAr : ar) {
            JButton b = new JButton(anAr + "%");
            b.setToolTipText(_I18N.getString("cutLevelPercentTip", anAr));
            b.setActionCommand(anAr);
            panel.add(b);
            b.addActionListener(ev -> setByPercent(Double.parseDouble(ev.getActionCommand())));
        }
        return panel;
    }


    /**
     * Make the dialog button panel
     */
    JPanel makeButtonPanel() {
        JPanel panel = new JPanel();
        panel.setLayout(new FlowLayout(FlowLayout.RIGHT));

        JButton resetButton = new JButton(_I18N.getString("reset"));
        resetButton.setToolTipText(_I18N.getString("resetCutLevelsTip"));
        panel.add(resetButton);
        resetButton.addActionListener(ev -> reset());

        JButton medianFilterButton = new JButton(_I18N.getString("medianFilter"));
        panel.add(medianFilterButton);
        medianFilterButton.setToolTipText(_I18N.getString("medianFilterTip"));
        medianFilterButton.addActionListener(ev -> medianFilter());

        JButton closeButton = new JButton(_I18N.getString("close"));
        closeButton.setToolTipText(_I18N.getString("closeTip"));
        panel.add(closeButton);
        closeButton.addActionListener(ev -> close());

        return panel;
    }


    /**
     * set the cut levels in the image to the selected values
     */
    void set() {
        imageProcessor.update();
    }


    /**
     * set the cut levels in the image to the selected values
     */
    void reset() {
        double minValue = imageProcessor.getMinValue();
        double maxValue = imageProcessor.getMaxValue();
        setCutLevels(minValue, maxValue);
    }

    /**
     * set the cut levels in the image to the given values
     */
    void setCutLevels(double minVal, double maxVal) {
        lowCut = minVal;
        highCut = maxVal;

        imageProcessor.setCutLevels(minVal, maxVal);
        imageProcessor.update();
    }


    /**
     * Automatically set the cut values by percent of distribution
     * that should be inside the cut levels.
     */
    void setByPercent(double percent) {
        imageProcessor.autoSetCutLevels(percent, imageDisplay.getVisibleArea());
        imageProcessor.update();
    }


    /**
     * Automatically set the cut levels using a median filtering algorithm
     */
    void medianFilter() {
        imageProcessor.autoSetCutLevels(imageDisplay.getVisibleArea());
        imageProcessor.update();
    }


    /**
     * Update the display to show the current cut levels and pixel distribution
     */
    void updateDisplay() {
        ignoreChangeEvents = true;

        try {
            lowCut = imageProcessor.getLowCut();
            highCut = imageProcessor.getHighCut();
            plotHistogram();
            double minValue = imageProcessor.getMinValue();
            double maxValue = imageProcessor.getMaxValue();
            if (minValue != maxValue) {
                rangeSlider.setBounds(minValue, maxValue);
                rangeSlider.setValues(lowCut, highCut);
            }
        } finally {
            ignoreChangeEvents = false;
        }
    }


    /**
     * Close the window
     */
    void close() {
        if (parent != null)
            parent.setVisible(false);
    }


    /**
     * Local class implementing the data source for the chart.
     */
    class SimpleDataset implements XYDataset {

        // the X values are the pixel values
        double[] xData;

        // the Y values are the number of pixels in a given range
        int[] yData;

        // Used to plot when there is no data
        public SimpleDataset() {
        }

        // plot the given data
        public SimpleDataset(double[] xData, int[] yData) {
            this.xData = xData;
            this.yData = yData;
        }

        /**
         * Returns the order of the domain (or X) values returned by the dataset.
         */
        public DomainOrder getDomainOrder() {
            return DomainOrder.ASCENDING;
        }

        /**
         * Returns the number of series in the data source;
         */
        public int getSeriesCount() {
            return 1;
        }

        @Override
        public Comparable getSeriesKey(int series) {
            return getSeriesName(series);
        }

        @Override
        public int indexOf(Comparable seriesKey) {
            return 0;
        }

        /**
         * Returns the name of the specified series.
         * @param seriesIndex The index of the required series (zero-based);
         */
        public String getSeriesName(int seriesIndex) {
            return "Pixels";
        }

        /**
         * Registers an object for notification of changes to the data source.
         * @param listener The object being registered;
         */
        public void addChangeListener(DatasetChangeListener listener) {
        }

        /**
         * Unregisters an object for notification of changes to the data source.
         * @param listener The object being unregistered;
         */
        public void removeChangeListener(DatasetChangeListener listener) {
        }

        /**
         * Returns the x-value for the specified series and item.  The implementation is responsible for
         * ensuring that the x-values are presented in ascending order.
         * @param seriesIndex The index of the series of interest (zero-based);
         * @param itemIndex The index of the item of interest (zero-based).
         */
        public Number getX(int seriesIndex, int itemIndex) {
            if (xData == null)
                return 0.0;
            return xData[itemIndex];
        }

        /**
         * Returns the y-value for the specified series and item.
         * @param seriesIndex The index of the series of interest (zero-based);
         * @param itemIndex The index of the item of interest (zero-based).
         */
        public Number getY(int seriesIndex, int itemIndex) {
            if (yData == null)
                return 0;
            return yData[itemIndex];
        }

        /**
         * Returns the x-value for the specified series and item.  The implementation is responsible for
         * ensuring that the x-values are presented in ascending order.
         * @param seriesIndex The index of the series of interest (zero-based);
         * @param itemIndex The index of the item of interest (zero-based).
         */
        public double getXValue(int seriesIndex, int itemIndex) {
            if (xData == null)
                return 0;
            return xData[itemIndex];
        }

        /**
         * Returns the y-value for the specified series and item.
         * @param seriesIndex The index of the series of interest (zero-based);
         * @param itemIndex The index of the item of interest (zero-based).
         */
        public double getYValue(int seriesIndex, int itemIndex) {
            if (yData == null)
                return 0;
            return yData[itemIndex];
        }

        /**
         * Returns the number of items in the specified series.
         * @param seriesIndex The index of the series of interest (zero-based).
         */
        public int getItemCount(int seriesIndex) {
            if (xData == null)
                return 1;
            return xData.length;
        }


        /** Returns the dataset group. */
        public DatasetGroup getGroup() {
            return null;
        }

        /** Sets the dataset group. */
        public void setGroup(DatasetGroup group) {
        }
    }
}
