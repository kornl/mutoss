package org.mutoss.gui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;

import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

public class MuTossAssumptionList extends JPanel {
	JList assumptionList = new JList();
	
	public MuTossAssumptionList() {
		GridBagConstraints c = new GridBagConstraints();	
		c.fill = GridBagConstraints.HORIZONTAL;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.weightx=1; c.weighty=1;
		c.insets = new Insets(5, 5, 5, 5); 
		setLayout(new GridBagLayout());		
		add(new JLabel("Assumptions"), c);
		c.fill = GridBagConstraints.BOTH;
		c.gridy++;
		add(new JScrollPane(assumptionList),c);
	}

}
