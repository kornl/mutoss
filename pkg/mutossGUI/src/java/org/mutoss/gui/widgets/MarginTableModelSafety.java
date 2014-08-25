package org.mutoss.gui.widgets;

import java.util.ArrayList;
import java.util.List;


public class MarginTableModelSafety extends MarginTableModel {
    private static final String[] header = {"Response", "Lower Margin", "Upper Margin"};

    private double[] lowersRatio   = new double[] {0, 1};
    private double[] uppersRatio   = new double[] {1, Double.POSITIVE_INFINITY};
    private double[] defaultsRatio = new double[] {0.5, 2};
    private double[] lowersDiff    = new double[] {Double.NEGATIVE_INFINITY, 0};
    private double[] uppersDiff    = new double[] {0, Double.POSITIVE_INFINITY};
    private double[] defaultsDiff  = new double[] {-10, 10};

//    public MarginTableModelSafety(List<String> vars, double[] lowerLimits, double upperLimits[]) {
//        super(header, vars, lowerLimits, upperLimits);
//    }
    

    public MarginTableModelSafety(boolean ratio) {
        this(new ArrayList<String>(), ratio);
    }

    public MarginTableModelSafety(List<String> vars, boolean ratio) {
        super(header, vars);
        if (ratio)
            setLimits(lowersRatio, uppersRatio, defaultsRatio);
        else
            setLimits(lowersDiff, uppersDiff, defaultsDiff);
    }

}



