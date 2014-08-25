package org.mutoss.gui.widgets;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import org.af.commons.widgets.MyJPopupMenu;
import org.af.commons.widgets.tables.ExtendedJTable;

public class PopulationsTable extends ExtendedJTable implements ActionListener {

	private MyJPopupMenu popupMenu;
	
    public PopulationsTable(List<String> header, List<Class> type, List<Double> defaults, 
    		List<Double> min, List<Double> max) {
        super(new PopulationsTableModel(header, type, defaults, min, max));
        setRowSelectionAllowed(true);
        getModel().addLine();        

        popupMenu = new MyJPopupMenu(new String[]{"Add Line", "Delete Line"}, new String[]{"add_line", "del_line"}, this);
        setComponentPopupMenu(popupMenu);
        setPreferredWidth(800);
    }

    public PopulationsTableModel getModel() {
        return (PopulationsTableModel) super.getModel();
    }

    public void actionPerformed(ActionEvent e) {
        if (e.getActionCommand().equals("add_line"))
            getModel().addLine();
        else if (e.getActionCommand().equals("del_line")) {
            int[] rows = getSelectedRows();
            for (int i : rows) {
                if (i >= 0 && i < getRowCount())
                    getModel().removeLine(i);
            }
        }
    }
}
