package org.mutoss.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;

import org.af.commons.Localizer;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mutoss.MethodHandler;
import org.mutoss.MuTossControl;

public class MuTossMenuBar extends JMenuBar implements ActionListener {

	protected static final Log logger = LogFactory.getLog(MuTossMenuBar.class);
	protected MuTossControl control;
    protected Localizer localizer = Localizer.getInstance();
    private JMenu menu;
    public final static String NOT_YET_IMPLEMENTED = "not yet implemented";

    public MuTossMenuBar() {
    	add(makeFileMenu());
    	if (System.getProperty("eclipse") != null) { 
    		add(makeMenuItem("R Shell", "rshell")); 
    		add(makeMenuItem("Throw R Error", "rerror"));
    	}
    }

    protected JMenuItem makeMenuItem(String text, String action) {
        return makeMenuItem(text, action, true);
    }

    protected JMenuItem makeMenuItem(String text, String action, boolean enabled) {
        JMenuItem item;        
        item = new JMenuItem(text);
        item.setActionCommand(action);
        item.addActionListener(this);
        item.setText(text);
        item.setEnabled(enabled);
        return (item);
    }


    public void enableAllMenus() {
        for (int i=0; i<getMenuCount(); i++) {
            getMenu(i).setEnabled(true);
        }
    }

    public void enableAllItems() {
        for (int i=0; i<getMenuCount(); i++) {
            enableAllItems(getMenu(i));
        }
    }

    public void enableAllItems(JMenu menu) {
    	//menu.getItemCount() returns the number of items on the menu, including separators.
        for (int i=0; i<menu.getItemCount(); i++) {
        	/* If the component at position i is not a menu item (like a separator),
        	   null is returned, so we must check this. */
            if (menu.getItem(i)!=null) menu.getItem(i).setEnabled(true);
        }
    }


    public void actionPerformed(ActionEvent e) {
    	logger.info("Got ActionCommand "+e.getActionCommand()+".");
    	if (e.getActionCommand().equals("rshell")) {
    		MuTossControl.console.setVisible(true);
    	} else if (e.getActionCommand().equals("rerror")) {
    		MuTossControl.getR().eval("1:20").asRChar();
    	} else if (e.getActionCommand().equals("muTossExport")) {
    		// MuTossExport
    	} else if (e.getActionCommand().equals("exit")) {
    		MuTossControl.getInstance().exit(true);
    	} else if (e.getActionCommand().equals("muTossFunctions")) {
    		MethodHandler.updateMethods();
    	} else {
    		JOptionPane.showMessageDialog(MuTossGUI.getGUI(), "Not yet implemented!", "Not yet implemented!", JOptionPane.INFORMATION_MESSAGE);
    	}
    }




	/****************************** Menus **********************************************/

    public JMenu makeFileMenu() {
    	JMenu menu = new JMenu("File");
    	menu.add(makeMenuItem("Load MuToss-Object", "loadRObj"));
    	menu.add(makeMenuItem("Export PDF File", "pdfExport"));
    	menu.add(makeMenuItem("Export MuToss-Object to R", "muTossExport"));
    	menu.add(makeMenuItem("Search for new MuToss functions", "muTossFunctions"));
    	menu.addSeparator();
    	menu.add(makeMenuItem("Exit", "exit"));
    	return menu;
    }


}
