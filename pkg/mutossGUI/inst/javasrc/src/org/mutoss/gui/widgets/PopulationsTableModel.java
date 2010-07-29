package org.mutoss.gui.widgets;

import java.util.List;
import java.util.Vector;

import javax.swing.table.AbstractTableModel;

import org.apache.commons.validator.routines.DoubleValidator;

public class PopulationsTableModel extends AbstractTableModel {



    List<String> header;
    List<Class> type;
    List<Double> defaults; 
	List<Double> min;
	List<Double> max;

	public List<List<Double>> data = new Vector<List<Double>>(); 

    public PopulationsTableModel(List<String> header, List<Class> type, List<Double> defaults, 
    		List<Double> min, List<Double> max) {
    	this.header = header;
    	this.type = type;
    	this.defaults = defaults;
    	this.min = min;
    	this.max = max;
    	
    	for (Class c : type) {
    		data.add(new Vector<Double>());
    	}

    }

    public int getColumnCount() {
        return header.size();
    }

    public int getRowCount() {
        return data.get(0).size();
    }

    public Object getValueAt(int r, int c) {
    	return data.get(c).get(r);
    }

    public void setValueAt(Object value, int row, int col) {
        if (value != null && value instanceof Double) {
            DoubleValidator validator = new DoubleValidator();
            Double d = validator.validate(value.toString());
            if (d > min.get(col) && d < max.get(col)) {
            	data.get(col).set(row, d);
            } else {
            	//TODO We should report an error!!!
            }
        }
    }


    public String getColumnName(int column) {
        return header.get(column);
    }


    public Class<?> getColumnClass(int columnIndex) {
        return Double.class;
    }

    public boolean isCellEditable(int rowIndex, int columnIndex) {
        return true;
    }

    public void addLine() {
    	int rowcount = getRowCount();
    	for (int i=0; i< data.size(); i++) {
    		if (rowcount>0) {
    			data.get(i).add(data.get(i).get(data.get(i).size()-1));
    		} else {
    			data.get(i).add(defaults.get(i));
    		}
    	}
        fireTableStructureChanged();
    }

    public void removeLine(int i) {
    	for (List<Double> row : data) {
    		row.remove(i);
    	}
        fireTableStructureChanged();
    }

}
