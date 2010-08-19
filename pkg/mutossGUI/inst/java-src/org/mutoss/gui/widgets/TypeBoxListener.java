package org.mutoss.gui.widgets;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.af.commons.widgets.validate.RealTextField;


public class TypeBoxListener implements ActionListener {
    private ROptionBox<String> typeBox;
    private RealTextField marginField;


    public TypeBoxListener(ROptionBox<String> typeBox, RealTextField marginField) {
        this.typeBox = typeBox;
        this.marginField = marginField;
        typeBox.addActionListener(this);
    }

    public void actionPerformed(ActionEvent e) {
        AnalysisDialogListenerFactory.updateMarginField(typeBox, marginField);
    }
}
