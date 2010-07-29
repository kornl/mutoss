package org.mutoss.gui;

import java.awt.Component;

import javax.swing.JTabbedPane;

public class MuTossTabbedPane extends JTabbedPane {
	public MuTossTabbedPane() {
		//ImageIcon icon = new ImageIcon(MuTossTabbedPane.class.getResource("images/middle.gif"));

		addTab("Output", null, OutputPanel.getOutputPanel(), "Output of the R functions");		

	}
	
	public void addTab(String title, Component component) {
        super.addTab(title, component); 
        showLastTab();
    }

	private void showLastTab() {
		this.setSelectedIndex(this.getTabCount()-1);		
	}

}
