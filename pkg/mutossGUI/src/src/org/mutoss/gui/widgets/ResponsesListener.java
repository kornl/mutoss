package org.mutoss.gui.widgets;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.af.commons.widgets.lists.SplitListChangeListener;
import org.af.jhlir.call.RLegalName;

abstract public class ResponsesListener implements SplitListChangeListener<RLegalName>, ActionListener {
    protected VarSelectSL slRespones;
    protected ROptionBox<String> cbType;
    protected MarginTable marginTable;

    public ResponsesListener(VarSelectSL slRespones, ROptionBox<String> cbType, MarginTable marginTable) {
        this.slRespones = slRespones;
        this.cbType = cbType;
        this.marginTable = marginTable;

        slRespones.addSplitListChangeListener(this);
        cbType.addActionListener(this);
    }


    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == cbType) {
            slRespones.allToLeft();
        }
    }
}
