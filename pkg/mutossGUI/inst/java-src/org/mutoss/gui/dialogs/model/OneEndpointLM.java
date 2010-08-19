package org.mutoss.gui.dialogs.model;

import java.awt.Component;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.JComboBox;
import javax.swing.JFrame;

import org.af.jhlir.call.RDataFrame;
import org.mutoss.MuTossControl;
import org.mutoss.gui.dialogs.StatMethodDialog;
import org.mutoss.gui.widgets.AnalysisDialogFactory;
import org.mutoss.gui.widgets.CovariatesSL;

public class OneEndpointLM extends StatMethodDialog implements ActionListener {

    private JComboBox cbResponse;
    private CovariatesSL slCovariates;

    public OneEndpointLM(JFrame frame, RDataFrame df) {
        super(frame, "Single Variate Linear Mode", df);
        cbResponse = AnalysisDialogFactory.makeNumericVarBox(df);
        slCovariates = AnalysisDialogFactory.makeCovariatesSL(df);
        
        String[] labels = {"Response", "Covariates"};
        Component[] comps = {cbResponse, slCovariates};
        
        // TODO Interaction

        setContent(labels, comps);

        setup();
    }
    
    protected void checkConstraints() {
        requiresNumVar();
        //requiresFactVar();
    }

    protected void onOk() {
    	String resp = cbResponse.getSelectedItem().toString();
    	List<String> covarName  = slCovariates.getSelectedStrings();
    	String rhs = covarName.get(0); // TODO handle list.size() = 0
    	for (int i=1; i<covarName.size(); i++) {
    		rhs += "+"+covarName.get(i);
    	}            
    	//TODO Bad hidden variables:
    	MuTossControl.getR().eval(".MuTossTMPModel <- lm("+resp+"~"+rhs+", data="+MuTossControl.getObj().getObjName()+"@data)");
    	MuTossControl.getObj().setModel(".MuTossTMPModel");
    	dispose();
    }

        
}

