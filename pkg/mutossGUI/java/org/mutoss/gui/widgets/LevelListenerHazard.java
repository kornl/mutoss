package org.mutoss.gui.widgets;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.af.commons.widgets.lists.SplitListChangeListener;

public class LevelListenerHazard implements ListSelectionListener, ActionListener, SplitListChangeListener<String> {

	protected LevelOrderSL slLevelOrder;
    protected ROptionBox<String> cbType;
    protected MarginTable marginTable;


    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == cbType) {
        	slLevelOrder.rightToLeft(slLevelOrder.rightList.getModel().getAllElements());
        }
    }
	
    public LevelListenerHazard(LevelOrderSL slLevelOrder, ROptionBox<String> cbType, MarginTable marginTable) {
    	this.cbType = cbType;
        this.marginTable = marginTable;
        this.slLevelOrder = slLevelOrder;
        
        slLevelOrder.rightList.addListSelectionListener(this);
        slLevelOrder.addSplitListChangeListener(this);
        cbType.addActionListener(this);
        updateMarginTableHazard(slLevelOrder.rightList, cbType, marginTable);
    }

    public void valueChanged(ListSelectionEvent e) {
    	updateMarginTableHazard(slLevelOrder.rightList, cbType, marginTable);
    }

	public void modelStateChanged(List<String> left, List<String> right) {
		updateMarginTableHazard(slLevelOrder.rightList, cbType, marginTable);		
	}
	
	public static void updateMarginTableHazard(JList responsesList, ROptionBox<String> typeBox, MarginTable marginTable) {
        List<String> r = new ArrayList<String>();
        
        for (int i=1; i < responsesList.getModel().getSize(); i++) { /* sic! */
            r.add(responsesList.getModel().getElementAt(0)+", "+responsesList.getModel().getElementAt(i));
        }
        String t = typeBox.getSelectedName();
        marginTable.setModel(new MarginTableModelHazard(r, t.equals("Ratio")));
    }
}
