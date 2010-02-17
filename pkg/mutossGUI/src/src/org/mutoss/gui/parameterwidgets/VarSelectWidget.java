package org.mutoss.gui.parameterwidgets;

import java.util.List;

import org.af.jhlir.call.RLegalName;
import org.af.jhlir.call.RNamedArgument;
import org.mutoss.MuTossControl;
import org.mutoss.gui.widgets.VarSelectBox;

public class VarSelectWidget extends VarSelectBox implements ParameterWidget {

	String name;
	
	public VarSelectWidget(String name, List<RLegalName> v) {		
		super(v);
		this.name = name;
	}
	
	@Override
	public Object getParameter() {	
		return new RNamedArgument(name, MuTossControl.getR().createRObject(this.getSelectedItem()));
	}

}
