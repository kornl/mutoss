package org.mutoss;

import org.af.jhlir.call.RList;
import org.af.jhlir.call.RObj;

public class Parameter {
	protected String name;
	protected boolean optional = false;
	protected String type;
	protected RObj defaultValue = null;
	protected String widget = null;
	protected String label = null;
	protected String fromR = null;
	protected String[] choices = null;
	
	public Parameter(String name, RList robj) {
		this.name = name;
		if (robj.get("optional")!=null && robj.get("optional").asRLogical().getData()[0]) {
			optional = true;
		}
		type = robj.get("type").asRChar().getData()[0];
		if (robj.get("default")!=null) {
			defaultValue = robj.get("default");
		}
		if (robj.get("widget")!=null) {
			widget = robj.get("widget").asRChar().getData()[0];
		}
		if (robj.get("label")!=null) {
			label = robj.get("label").asRChar().getData()[0];
		} else {
			label = name;
		}
		if (robj.get("choices")!=null) {
			choices = robj.get("choices").asRChar().getData();
		}
		if (robj.get("fromR")!=null) {
			fromR = robj.get("fromR").asRChar().getData()[0];
		}
	}

	public String getName() {
		return name;
	}

	public boolean isOptional() {
		return optional;
	}

	public String getType() {
		return type;
	}

	public RObj getDefaultValue() {
		return defaultValue;
	}

	public boolean isAdditional() {		
		return !(name.equals("pValues") || 
				name.equals("adjPValues") || 
				name.equals("alpha") ||
				name.equals("model") ||
				name.equals("data") ||
				name.equals("hypotheses"));
	}

	public String getLabel() {		
		return label;
	}

	public String[] getChoices() {		
		return choices;
	}

	public String getFromR() {
		return fromR;
	}
}
