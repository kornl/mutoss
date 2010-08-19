package org.mutoss.gui.widgets;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import org.af.jhlir.call.RDataFrame;

public class LevelSelectionListener implements ActionListener {
    private RDataFrame df;
    private VarSelectBox factorBox;
    private LevelSelectBox levelBox;


    public LevelSelectionListener(RDataFrame df, VarSelectBox factorBox, LevelSelectBox levelBox) {
        this.df = df;
        this.factorBox = factorBox;
        this.levelBox = levelBox;
        factorBox.addActionListener(this);
        AnalysisDialogListenerFactory.updateLevelSelectionBox(df, factorBox, levelBox);
    }

//    public LevelSelectionListener(DfRefW df, VarSelectBox factorBox, JComboBox levelBox) {
//        this.df = df;
//        this.factorBox = factorBox;
//        this.levelBox = levelBox;
//        factorBox.addActionListener(this);
//    }

    public void actionPerformed(ActionEvent e) {
        AnalysisDialogListenerFactory.updateLevelSelectionBox(df, factorBox, levelBox);
    }
}
