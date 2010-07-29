package org.mutoss.gui.widgets;

import java.util.ArrayList;
import java.util.List;

public class MarginTableModelHazard extends MarginTableModel{
    private static final String[] header = {"Response", "Margin"};

    private double[] lowersRatio   = new double[] {0};
    private double[] uppersRatio   = new double[] {Double.POSITIVE_INFINITY};
    private double[] defaultsRatio = new double[] {1};
    private double[] lowersDiff    = new double[] {Double.NEGATIVE_INFINITY};
    private double[] uppersDiff    = new double[] {Double.POSITIVE_INFINITY};
    private double[] defaultsDiff  = new double[] {0};

//    public MarginTableModelHazard(List<String> vars, double[] lowerLimits, double upperLimits[]) {
//        super(header, vars, lowerLimits, upperLimits);
//    }

    public MarginTableModelHazard(boolean ratio) {
        this(new ArrayList<String>(), ratio);
    }

    public MarginTableModelHazard(List<String> vars, boolean ratio) {
        super(header, vars);
        if (ratio)
            setLimits(lowersRatio, uppersRatio, defaultsRatio);
        else
            setLimits(lowersDiff, uppersDiff, defaultsDiff);
    }
}
