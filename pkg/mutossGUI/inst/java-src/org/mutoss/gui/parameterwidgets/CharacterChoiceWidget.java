package org.mutoss.gui.parameterwidgets;

import javax.swing.JComboBox;

import org.af.jhlir.call.RNamedArgument;
import org.mutoss.MuTossControl;

public class CharacterChoiceWidget extends JComboBox implements ParameterWidget {

	String name;
	
	public CharacterChoiceWidget(String name, String[] choices) {
		super(choices);
		this.name = name;
	}
	
	public Object getParameter() {	
		return new RNamedArgument(name, MuTossControl.getR().createRObject(this.getSelectedItem().toString()));
	}

}
