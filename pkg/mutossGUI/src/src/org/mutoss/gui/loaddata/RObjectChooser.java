package org.mutoss.gui.loaddata;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;

import org.af.commons.widgets.buttons.HorizontalButtonPane;
import org.af.commons.widgets.buttons.OkCancelButtonPane;
import org.af.jhlir.call.RObj;
import org.mutoss.MuTossControl;


public class RObjectChooser extends JDialog implements ActionListener {

	public RObjectChooser(JFrame parent) {
		this(parent, "Choose R Object");		
	}
	
	JList list;	
	
	public RObjectChooser(JFrame parent, String title) {
		super(parent, title, true);
		
		createContent();
		
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.insets = new Insets(5, 5, 5, 5); 
		c.weightx=1; c.weighty=1;
		
		getContentPane().setLayout(new GridBagLayout());
		getContentPane().add(new JScrollPane(list), c);		
		this.setSize(450, 300);
		this.setLocationRelativeTo(parent);		
		c.gridy++;		
		OkCancelButtonPane buttonPane = new OkCancelButtonPane();
		buttonPane.addActionListener(this);
		getContentPane().add(buttonPane, c);
		setVisible(true);
	}

	private void createContent() {
		RObj obj = MuTossControl.getR().eval("ls()");
		list = new JList(obj.asRChar().getData());		
	}
	
	String selected = null;

	public String getSelected() {
		return selected;
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals(HorizontalButtonPane.OK_CMD)) {
			if (list.getSelectedIndex() == -1) {
				JOptionPane.showMessageDialog(this.getParent(), "You have not selected an R object.", "No R objected selected", JOptionPane.INFORMATION_MESSAGE);
			} else {
				selected = (String)list.getSelectedValue();
				dispose();
			}
		} else if (e.getActionCommand().equals(HorizontalButtonPane.CANCEL_CMD)) {
			dispose();
		}
	}
	
}
