package org.mutoss.gui.widgets;

import java.util.Arrays;
import java.util.Hashtable;
import java.util.List;

import javax.swing.table.AbstractTableModel;

import org.apache.commons.validator.routines.DoubleValidator;

public class MarginTableModel extends AbstractTableModel {
    /**
     *
     */
    private static final long serialVersionUID = 1L;
    private List<String> vars;
//    private String[] header = {"Response", "Lower", "Upper"};
    private String[] header;
    private Hashtable<String, Double>[] toMarginValue;
    private double[] lowerLimits;
    private double[] upperLimits;
    private double[] defaults;

//    public MarginTableModel(String header[], double[] lowerLimits, double[] upperLimits) {
//        this(header, new ArrayList<String>(), lowerLimits, upperLimits);
//    }

    public MarginTableModel(String header[], List<String> vars) {
        this.header = header;
        this.vars = vars;
        toMarginValue = new Hashtable[header.length - 1]; 
        for (int i=0; i<header.length-1; i++) {
            toMarginValue[i] = new Hashtable<String, Double>();
            for (String s : vars) {
                toMarginValue[i].put(s, 0d);
            }
        }
    }

    protected static double[] getLimitArray(int n, double val) {
        double[] d = new double[n];
        Arrays.fill(d, val);
        return d;
    }

    public int getColumnCount() {
        return header.length;
    }

    public int getRowCount() {
        return vars.size();
    }

    public Object getValueAt(int rowIndex, int columnIndex) {
        String s = vars.get(rowIndex);
        if (columnIndex == 0)
            return s;
        else
            return toMarginValue[columnIndex-1].get(s);
    }

    public void setValueAt(Object value, int row, int col) {
        if (value != null && value instanceof Number) {
            String s = vars.get(row);
            if (col > 0) {
                DoubleValidator validator = new DoubleValidator();
                Double d = validator.validate(value.toString());
                if (d != null && d > lowerLimits[col-1] && d < upperLimits[col-1]) {
                    toMarginValue[col-1].put(s, d);
                    fireTableCellUpdated(row, col);
                }
            }

        }
    }


    public String getColumnName(int column) {
        return header[column];
    }


    public Class<?> getColumnClass(int columnIndex) {
        if (columnIndex == 0)
            return String.class;
        else
            return Double.class;
    }

    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return columnIndex > 0;
    }

    public void setLimits(double[] lowerLimits, double[] upperLimits, double[] defaults) {
        this.lowerLimits = lowerLimits;
        this.upperLimits = upperLimits;
        this.defaults = defaults;
        for (int i=0; i<header.length-1; i++) {
            for (String s : vars) {
                toMarginValue[i].put(s, defaults[i]);
            }
        }
    }

}



