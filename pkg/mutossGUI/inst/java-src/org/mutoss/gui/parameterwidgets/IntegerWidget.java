package org.mutoss.gui.parameterwidgets;

import org.af.commons.widgets.validate.IntegerTextField;
import org.af.commons.widgets.validate.ValidationException;
import org.af.jhlir.call.RNamedArgument;
import org.mutoss.MuTossControl;

public class IntegerWidget extends IntegerTextField implements ParameterWidget {

	public IntegerWidget(String name) {
		super(name);
		setColumns(10);
		// TODO Auto-generated constructor stub
	}

	public Object getParameter() {
		double val;
		try {
			val = this.getValidatedValue();
		} catch (ValidationException e) {
			//TODO What is the best way to handle wrong input?
			return null;
		}
		return new RNamedArgument(this.descriptiveName, MuTossControl.getR().createRObject(new double[] {val}));
	}

}
