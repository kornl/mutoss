package org.mutoss.gui.widgets;

import java.util.List;

import org.af.jhlir.call.RLegalName;

public class ResponseBox extends VarSelectBox {
    public ResponseBox(List<RLegalName> objects) {
        super(objects);
    }

    public ResponseBox(RLegalName[] objects) {
        super(objects);
    }
}
