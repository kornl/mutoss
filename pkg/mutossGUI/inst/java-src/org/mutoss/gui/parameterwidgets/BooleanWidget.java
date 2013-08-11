package org.mutoss.gui.parameterwidgets;

import javax.swing.JCheckBox;

import org.af.jhlir.call.RNamedArgument;
import org.mutoss.MuTossControl;

public class BooleanWidget extends JCheckBox implements ParameterWidget {

	protected String name;
	
	public BooleanWidget(String name) {
		super(name);
		this.name = name;
	}

	public Object getParameter() {		
		return new RNamedArgument(this.name, MuTossControl.getR().createRObject(new boolean[] {this.isSelected()}));
	}

}
