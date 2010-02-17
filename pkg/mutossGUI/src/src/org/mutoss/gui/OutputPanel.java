package org.mutoss.gui;

import java.awt.FileDialog;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.af.commons.widgets.HTMLPane;
import org.rosuda.JRI.RMainLoopCallbacks;
import org.rosuda.JRI.Rengine;

public class OutputPanel extends JPanel implements RMainLoopCallbacks {
	
	static OutputPanel panel = null;
	
	public static OutputPanel getOutputPanel() {
		if (panel==null) panel = new OutputPanel();
		return panel;
	}
	
	HTMLPane taOutput = new HTMLPane();
	
	public HTMLPane getOutputPane() {
		return taOutput;
	}

	protected OutputPanel() {
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.weightx=1; c.weighty=1;
		setLayout(new GridBagLayout());
		
		this.add(new JScrollPane(taOutput), c);
		taOutput.setAlignmentX(LEFT_ALIGNMENT);
	}

	@Override
	public void rBusy(Rengine arg0, int which) {
		taOutput.appendParagraph("rBusy("+which+")");		
	}

	@Override
	public String rChooseFile(Rengine re, int newFile) {
		FileDialog fd = new FileDialog(MuTossGUI.getGUI(), (newFile==0)?"Select a file":"Select a new file", (newFile==0)?FileDialog.LOAD:FileDialog.SAVE);
		fd.show();
		String res=null;
		if (fd.getDirectory()!=null) res=fd.getDirectory();
		if (fd.getFile()!=null) res=(res==null)?fd.getFile():(res+fd.getFile());
		return res;
	}

	@Override
	public void rFlushConsole(Rengine arg0) {}

	@Override
	public void rLoadHistory(Rengine arg0, String arg1) {}

	@Override
	public String rReadConsole(Rengine re, String prompt, int addToHistory) {
		System.out.print(prompt);
		try {
			BufferedReader br=new BufferedReader(new InputStreamReader(System.in));
			String s=br.readLine();
			return (s==null||s.length()==0)?s:s+"\n";
		} catch (Exception e) {
			System.out.println("jriReadConsole exception: "+e.getMessage());
		}
		return null;
	}

	@Override
	public void rSaveHistory(Rengine arg0, String arg1) {}

	@Override
	public void rShowMessage(Rengine re, String message) {
		taOutput.appendParagraph("rShowMessage \""+message+"\"");
	}

	@Override
	public void rWriteConsole(Rengine re, String text, int oType) {
		taOutput.appendParagraph(text);
	}

	public static void addOutput(String ro) {
		getOutputPanel().getOutputPane().appendHTML(ro);		
	}

	public static void close() {
		panel = null;		
	}
	
	
}
