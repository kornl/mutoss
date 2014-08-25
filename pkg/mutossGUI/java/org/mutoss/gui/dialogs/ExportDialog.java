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
import javax.swing.JTextField;

import org.af.commons.widgets.buttons.HorizontalButtonPane;
import org.af.commons.widgets.buttons.OkCancelButtonPane;
import org.af.commons.widgets.validate.RealTextField;
import org.af.commons.widgets.validate.ValidationException;
import org.mutoss.MuTossControl;


public class ExportDialog extends JDialog implements ActionListener {

	public ExportDialog(JFrame parent) {
		this(parent, "Set Alpha");		
	}
	
	JTextField tfExport;
	String type;
	
	public ExportDialog(JFrame parent, String type) {
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
		
		String label = "R name";
		
		getContentPane().add(new JLabel(label), c);
		c.gridx++;
		getContentPane().add(tfExport, c);
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
		tfExport = new RealTextField("alpha", 0, 1);
		tfExport.setText("0.05");
	}
	
	String selected = null;

	public String getSelected() {
		return selected;
	}

	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals(HorizontalButtonPane.OK_CMD)) {
			try {
				String exportName = tfExport.getText();
				MuTossControl.getObj().exportAs(exportName);
				dispose();
			} catch (ValidationException e1) {
				JOptionPane.showMessageDialog(this.getParent(), "You have to choose a legal R name.", "No legal R name", JOptionPane.ERROR_MESSAGE);
			}
		} else if (e.getActionCommand().equals(HorizontalButtonPane.CANCEL_CMD)) {
			dispose();
		}
	}
	
}
