package org.mutoss.gui.dialogs;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;

import org.af.commons.widgets.buttons.HorizontalButtonPane;
import org.af.commons.widgets.buttons.OkCancelButtonPane;
import org.af.commons.widgets.validate.RealTextField;
import org.af.commons.widgets.validate.ValidationException;
import org.mutoss.MuTossControl;


public class AlphaDialog extends JDialog implements ActionListener {

	public AlphaDialog(JFrame parent) {
		this(parent, "Set Alpha");		
	}
	
	RealTextField tfAlpha;
	String type;
	
	public AlphaDialog(JFrame parent, String type) {
		super(parent, type, true);
		this.type = type;
		
		createContent();
		
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.insets = new Insets(5, 5, 5, 5); 
		c.weightx=1; c.weighty=1;
		
		getContentPane().setLayout(new GridBagLayout());
		
		String label = null;
		
		if (type.equals("FWER")) label = "Family wise error rate alpha";
		if (type.equals("GFWER")) label = "Generalized family wise error rate alpha";
		if (type.equals("FDR")) label = "False discovery rate alpha";
		
		getContentPane().add(new JLabel(label), c);
		c.gridx++;
		getContentPane().add(tfAlpha, c);
		c.gridy++; c.gridx=0;				
		OkCancelButtonPane buttonPane = new OkCancelButtonPane();
		buttonPane.addActionListener(this);
		c.gridwidth = 2;
		getContentPane().add(buttonPane, c);
		pack();
		this.setLocationRelativeTo(parent);
		setVisible(true);
	}

	private void createContent() {
		tfAlpha = new RealTextField("alpha", 0, 1);
		tfAlpha.setText("0.05");
	}
	
	String selected = null;

	public String getSelected() {
		return selected;
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals(HorizontalButtonPane.OK_CMD)) {
			try {
				Double alpha = tfAlpha.getValidatedValue();
				MuTossControl.getObj().setAlpha(type, alpha);
				dispose();
			} catch (ValidationException e1) {
				JOptionPane.showMessageDialog(this.getParent(), "You have to choose an error rate between 0 and 1.", "No valid error rate", JOptionPane.ERROR_MESSAGE);
			}
		} else if (e.getActionCommand().equals(HorizontalButtonPane.CANCEL_CMD)) {
			dispose();
		}
	}
	
}
