package org.mutoss.gui;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;

import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.UIManager;

import org.af.commons.Localizer;
import org.af.commons.errorhandling.ErrorDialog;
import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.logging.ApplicationLog;
import org.af.commons.logging.LoggingSystem;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mutoss.MuTossControl;

public class MuTossGUI extends JFrame implements WindowListener {

	protected static MuTossGUI gui = null;
	protected MuTossMainPanel mpanel;
	protected MuTossTabbedPane tabbedPane;
	private static final Log logger = LogFactory.getLog(MuTossGUI.class);
	
	public MuTossTabbedPane getTabbedPane() {
		return tabbedPane;
	}

	public MuTossMainPanel getMpanel() {
		return mpanel;
	}

	public static void startGUI() {
		//UIManager.put("control", Color.PINK);
		//UIManager.put("Panel.background", Color.PINK);
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				getGUI().setVisible(true);
			}
		});		
	}
	
	public static MuTossGUI getGUI() {
		if (gui==null) {
			gui = new MuTossGUI();
		}
		return gui;
	}
	
	protected MuTossGUI() {		
		super("MuToss GUI");
		setIconImage((new ImageIcon(getClass().getResource("/org/mutoss/images/mutoss.png"))).getImage());
		
		String loggingProperties = "/commons-logging.properties";
		if (System.getProperty("eclipse") != null) { loggingProperties = "/commons-logging-verbose.properties"; }
		
		if (!LoggingSystem.alreadyInitiated()) {
			LoggingSystem.init(
					loggingProperties,
					false,
					true,
					new ApplicationLog());
			ErrorHandler.init("rohmeyer@small-projects.de", "http://www.algorithm-forge.com/report/bugreport.php", true, true, ErrorDialog.class);
		}
		
		System.setOut(new PrintStream(new LoggingOutputStream(logger), true));
		
		Localizer.getInstance().addResourceBundle("org.mutoss.gui.widgets.ResourceBundle");
		
		// Fenster in der Mitte des Bildschirms platzieren mit inset = 50 Pixeln Rand.
		int inset = 50;
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		setBounds(inset, inset,
				screenSize.width  - inset*2,
				screenSize.height - inset*2);

		setJMenuBar(new MuTossMenuBar());
		setContent();		
		setVisible(true);
		MuTossControl.getR();
		addWindowListener(this);
	}
	
	public JPanel leftPanel() {
		JPanel panel = new JPanel();
		GridBagConstraints c = new GridBagConstraints();
		mpanel = new MuTossMainPanel();
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.weightx=1; c.weighty=1;
		panel.setLayout(new GridBagLayout());
		panel.add(mpanel, c);
		c.gridy++;
		panel.add(new MuTossAssumptionList(), c);
		return panel;
	}

	public void setContent() {
		tabbedPane = new MuTossTabbedPane();
		JSplitPane splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				new JScrollPane(leftPanel()), tabbedPane);
		splitPane.setOneTouchExpandable(true);
		splitPane.setDividerLocation(350);

		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.weightx=1; c.weighty=1;
		this.getContentPane().setLayout(new GridBagLayout());	

		this.getContentPane().add(splitPane, c);		
		
	}

	public static void main(String[] args) {
		startGUI();
	}

	@Override
	public void windowActivated(WindowEvent arg0) {}

	@Override
	public void windowClosed(WindowEvent arg0) {
		MuTossControl.exit(false);
		exit();		
	}

	@Override
	public void windowClosing(WindowEvent arg0) {}

	@Override
	public void windowDeactivated(WindowEvent arg0) {}

	@Override
	public void windowDeiconified(WindowEvent arg0) {}

	@Override
	public void windowIconified(WindowEvent arg0) {}

	@Override
	public void windowOpened(WindowEvent arg0) {}

	public static void exit() {
		if (gui!=null) {
			gui.dispose();
		}
		gui = null;		
	}

} 

class LoggingOutputStream extends ByteArrayOutputStream { 
	 
    private String lineSeparator;    
    Log logger;
 
    public LoggingOutputStream(Log logger) { 
        super(); 
        this.logger = logger; 
        lineSeparator = System.getProperty("line.separator"); 
    } 
 
    public void flush() throws IOException { 
        String record; 
        synchronized(this) { 
            super.flush(); 
            record = this.toString(); 
            super.reset(); 
            if (record.length() == 0 || record.equals(lineSeparator)) return; 
            logger.info(record); 
        } 
    } 
} 
