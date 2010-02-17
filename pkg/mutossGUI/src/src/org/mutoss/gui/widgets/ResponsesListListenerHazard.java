package org.mutoss.gui.widgets;

import java.util.List;

import org.af.jhlir.call.RLegalName;

public class ResponsesListListenerHazard extends ResponsesListener {

    public ResponsesListListenerHazard(VarSelectSL slResponses, ROptionBox<String> cbType, MarginTable marginTable) {
        super(slResponses, cbType, marginTable);
    }

    public void modelStateChanged(List<RLegalName> left, List<RLegalName> right) {
        AnalysisDialogListenerFactory.updateMarginTableHazard(slRespones, cbType, marginTable);
    }
}
