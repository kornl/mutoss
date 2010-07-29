package org.mutoss;

import java.util.Comparator;

public class MethodSorter implements Comparator<Method> {

	@Override
	public int compare(Method o1, Method o2) {		
		return o1.label.compareTo(o2.label);
	}

}
