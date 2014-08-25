package org.mutoss.gui.parameterwidgets;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

import org.af.jhlir.call.RNamedArgument;
import org.af.jhlir.call.RObj;
import org.mutoss.MuTossControl;
import org.mutoss.gui.MuTossGUI;
import org.mutoss.gui.loaddata.RObjectChooser;

public class RObjectWidget extends JPanel implements ParameterWidget, ActionListener {

	String name;
	RObj obj;
	JButton choose = new JButton("Select Object");
	JTextField tf = new JTextField(30);
	
	public RObjectWidget(String name) {		
		this.name = name;
		
		setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();		
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.insets = new Insets(5, 5, 5, 5); 
		c.weightx=1; c.weighty=1;
		
		add(tf,c);
		
		c.gridx++;
		c.weightx=0; c.weighty=0;
		add(choose,c);
		
		choose.addActionListener(this);
	}
	
	public RNamedArgument getParameter() {	
		return new RNamedArgument(name, MuTossControl.getR().eval(tf.getText()));
	}

	public void actionPerformed(ActionEvent e) {
		RObjectChooser roc = new RObjectChooser(MuTossGUI.getGUI());
		if (roc.getSelected() != null) {
			tf.setText(roc.getSelected());				
		}	
		
	}

}
