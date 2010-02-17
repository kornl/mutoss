package org.mutoss.gui.widgets;

import java.util.List;
import java.util.Vector;

import org.af.commons.widgets.tables.ExtendedJTable;

public class MarginTable extends ExtendedJTable {

	
	public MarginTable(MarginTableModel model) {
        super(model);
//        this(header, getLimitArray(header.length-1, Double.NEGATIVE_INFINITY), getLimitArray(header.length-1, Double.POSITIVE_INFINITY));
        setVisibleRowCount(4);
        setRowSelectionAllowed(false);
        setColumnSelectionAllowed(false);
        setCellSelectionEnabled(false);
    }
    
    public List<Double> getColumn(int i) {
    	Vector<Double> column = new Vector<Double>();
    	for(int j=0; j<getModel().getRowCount(); j++) {
    		Double d = (Double)(getModel().getValueAt(j, i));
    		column.add(d);
    	}    	
    	return column;
    }



//    public MarginTable(String[] header, double[] lowers, double uppers[]) {
//        super(new MarginTableModel(header, lowers, uppers));
////        getColumnModel().getColumn(1).setCellEditor(new DefaultCellEditor(new RealTextField("Lower Margin", lower, upper)));
//    }

    public MarginTableModel getModel() {
        return (MarginTableModel)super.getModel();
    }
}
