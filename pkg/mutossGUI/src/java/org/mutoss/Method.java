package org.mutoss;

import java.util.Arrays;
import java.util.List;
import java.util.Vector;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.jhlir.call.RList;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

public class Method {
	
	private static final Log logger = LogFactory.getLog(Method.class);
	protected String name; // obligatory
	protected String label; // obligatory
	protected String errorControl = null; // optional
	protected String function;	// obligatory
	protected List<Parameter> parameters = new Vector<Parameter>();	// obligatory
	protected List<Parameter> input = new Vector<Parameter>();
	protected List<String> output; // obligatory
	protected String info; // obligatory
	
	public Method(String method) {
		name = method;
		MuTossControl.getR().eval(".MuTossMethod <- mutoss:::"+name+"()");
		label = MuTossControl.getR().eval(".MuTossMethod@label").asRChar().getData()[0];
		try {
			errorControl = MuTossControl.getR().eval(".MuTossMethod@errorControl").asRChar().getData()[0];
		} catch(ArrayIndexOutOfBoundsException e) {
			logger.info("This method does not specify the error control");
		}
		function = MuTossControl.getR().eval(".MuTossMethod@callFunction").asRChar().getData()[0];
		RList rParameters = MuTossControl.getR().eval(".MuTossMethod@parameters").asRList();		
		for (int i=0; i < rParameters.getLength(); i++) {
			if (!(rParameters.get(i) instanceof RList)) {
				ErrorHandler.getInstance().makeErrDialog("Arguments must be of type list! Please write the author of "+method+".", null, false);
			}
			parameters.add(new Parameter(rParameters.getName(i), (RList) rParameters.get(i)));
		}
		List <String> slots = Arrays.asList(new String[] {"data", "model", "statistic", "hypNames", "pValues", "adjPValues", "errorControl", "rejected", "qValues", "locFDR", "pi0", "confIntervals", "alpha", "k", "q"}); 
		for (Parameter param : parameters) {
			if (slots.contains(param.getName())) {
				input.add(param);
			}
		}
		output = Arrays.asList(MuTossControl.getR().eval(".MuTossMethod@output").asRChar().getData());
		info = MuTossControl.getR().eval(".MuTossMethod@info").asRChar().getData()[0];
	}
		
	public boolean isApplicable() {		
		logger.debug("Checking whether Method \""+name+"\" is applicable.");
		for (Parameter i : input) {
			if (i.isOptional()) continue;
			if (i.getName().equals("pValues") && !MuTossControl.getObj().hasPValues()) return false;
			if (i.getName().equals("data") && !MuTossControl.getObj().hasData()) return false;
			if (i.getName().equals("model") && !MuTossControl.getObj().hasModel()) return false;
			if (i.getName().equals("adjPValues") && !MuTossControl.getObj().hasAdjPValues()) return false;			
			if ((i.getName().equals("alpha") || i.getName().equals("k") || i.getName().equals("q")) && !MuTossControl.getObj().hasErrorRate()) return false;			
		}
		if (errorControl!=null && MuTossControl.getObj().hasErrorRate()) {
			if (!errorControl.equals(MuTossControl.getObj().getErrorControlType())) return false;
		}
		if (MuTossControl.getR().eval("exists(\""+name+".model\")").asRLogical().getData()[0]) {
			if (!MuTossControl.getR().eval(""+name+".model("+MuTossControl.getObj().getObjName()+"@model)").asRLogical().getData()[0]) return false;
		}
		// Here come the assumptions:
		logger.debug("Method \""+name+"\" is applicable");
		return true;
	}

	public boolean returnsAdjPValues() {
		return output.contains("adjPValues");
	}

	public boolean returnsRejected() {		
		return output.contains("rejected");
	}
	
	public boolean returnsPValues() {
		return output.contains("pValues");
	}
	
	public String getName() {
		return name;
	}

	public String getLabel() {
		return label;
	}

	public String getErrorControl() {
		return errorControl;
	}

	public String getFunction() {
		return function;
	}

	public List<Parameter> getInput() {
		return input;
	}

	public List<String> getOutput() {
		return output;
	}

	public String getInfo() {
		return info;
	}

	public List<Parameter> getParameters() {		
		return parameters;
	}

	public boolean returnsCI() {
		return output.contains("confIntervals");
	}

}
