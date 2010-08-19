package org.mutoss;

import java.util.Hashtable;

import org.af.commons.tools.StringTools;
import org.af.jhlir.call.RObj;

public class Model {


	public final String NO_FORMULA = "no formula available";

	protected String rHSide = NO_FORMULA;
	protected String lHSide = NO_FORMULA;
	protected String[] classStrings = new String[0];
	protected String rName;
	protected Hashtable<String,String> dataClasses = new Hashtable<String,String>();

	public Model(String rName) {
		this.rName = rName;
		RObj formula = MuTossControl.getR().eval("as.character(formula("+rName+"))");
		if (formula!=null) {			
			lHSide = formula.asRChar().getData()[1];
			rHSide = formula.asRChar().getData()[2];
		}
		RObj classes = MuTossControl.getR().eval("class("+rName+")");
		if (classes!=null) {
			classStrings = classes.asRChar().getData();
		}		
		/* terms is a cool function - does it work with all (most) models? */
		RObj rdataClasses = MuTossControl.getR().eval("attr(terms("+rName+"), \"dataClasses\")");
		if (rdataClasses!=null) {
			String[] variables = MuTossControl.getR().eval("names(attr(terms("+rName+"), \"dataClasses\"))").asRChar().getData();
			String[] classArray = rdataClasses.asRChar().getData();
			for (int i=0; i<variables.length; i++) {
				dataClasses.put(variables[i], classArray[i]);
			}
		}
	}
	
	public String toString() {
		return "R model "+rName+" (Formula: "+lHSide+"~"+rHSide+"; Class: "+StringTools.arrayToString(classStrings)+")";
	}
	
	public RObj getRModel() {
		return MuTossControl.getR().eval(rName);
	}

}
