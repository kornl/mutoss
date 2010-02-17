package org.mutoss.gui.widgets;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JList;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import org.af.commons.widgets.lists.SplitListChangeListener;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class LevelListenerSafety implements ListSelectionListener, ActionListener, SplitListChangeListener<String> {
	private static final Log logger = LogFactory.getLog(LevelListenerSafety.class);	
	
	LevelOrderSL slLevelOrder;
    protected ROptionBox<String> cbType;
    protected MarginTable marginTable;
	
    public LevelListenerSafety(LevelOrderSL slLevelOrder, ROptionBox<String> cbType, MarginTable marginTable) {
        this.slLevelOrder = slLevelOrder;
        this.cbType = cbType;
        this.marginTable = marginTable;

        slLevelOrder.rightList.addListSelectionListener(this);
        slLevelOrder.addSplitListChangeListener(this);
        cbType.addActionListener(this);
        updateMarginTableSafety(slLevelOrder.rightList, cbType, marginTable);
    }

    public void valueChanged(ListSelectionEvent e) {    	
        updateMarginTableSafety(slLevelOrder.rightList, cbType, marginTable);
    }
    
    public static void updateMarginTableSafety(JList responsesList, ROptionBox<String> typeBox, MarginTable marginTable) {
        List<String> r = new ArrayList<String>();
        
        for (int i=1; i < responsesList.getModel().getSize(); i++) { /* sic! */
            r.add(responsesList.getModel().getElementAt(0)+", "+responsesList.getModel().getElementAt(i));
        }
        String t = typeBox.getSelectedName();
        marginTable.setModel(new MarginTableModelSafety(r, t.equals("Ratio")));
    }
    
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == cbType) {
        	slLevelOrder.rightToLeft(slLevelOrder.rightList.getModel().getAllElements());
        }
    }

	public void modelStateChanged(List<String> left, List<String> right) {
		updateMarginTableSafety(slLevelOrder.rightList, cbType, marginTable);
	}
}

