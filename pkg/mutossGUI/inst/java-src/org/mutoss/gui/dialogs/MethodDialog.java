package org.mutoss.gui.dialogs;

import java.awt.Component;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JViewport;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.commons.widgets.HTMLPaneWithButtons;
import org.af.commons.widgets.buttons.HorizontalButtonPane;
import org.af.commons.widgets.buttons.OkCancelButtonPane;
import org.af.jhlir.call.RLegalName;
import org.af.jhlir.call.RList;
import org.af.jhlir.call.RNamedArgument;
import org.af.jhlir.call.RObj;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mutoss.Method;
import org.mutoss.MuTossControl;
import org.mutoss.Parameter;
import org.mutoss.gui.OutputPanel;
import org.mutoss.gui.parameterwidgets.BooleanWidget;
import org.mutoss.gui.parameterwidgets.CharacterChoiceWidget;
import org.mutoss.gui.parameterwidgets.IntegerWidget;
import org.mutoss.gui.parameterwidgets.NumberWidget;
import org.mutoss.gui.parameterwidgets.ParameterWidget;
import org.mutoss.gui.parameterwidgets.RObjectWidget;
import org.mutoss.gui.parameterwidgets.VarSelectWidget;


public class MethodDialog extends JDialog implements ActionListener {

	private static final Log logger = LogFactory.getLog(MethodDialog.class);
	
	Method method;
	
	HTMLPaneWithButtons infopane;
	
	Hashtable<String, ParameterWidget> ht = new Hashtable<String, ParameterWidget>(); 
	Hashtable<String, JCheckBox> htC = new Hashtable<String, JCheckBox>();
	
	public MethodDialog(JFrame parent, Method method) {
		super(parent, "Method: "+method.getLabel());	
		this.method = method;
		
		createContent();
		
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.insets = new Insets(5, 5, 5, 5); 
		c.weightx=0.5; c.weighty=1;
		
		getContentPane().setLayout(new GridBagLayout());
		
		getContentPane().add(getArgumentPanel(), c);
		c.gridx++;c.weightx=1;
		JScrollPane scrollPane = new JScrollPane(infopane);
		getContentPane().add(scrollPane, c);
		c.gridy++; 
		
		c.gridx=0;				
		OkCancelButtonPane buttonPane = new OkCancelButtonPane();
		buttonPane.addActionListener(this);
		c.gridwidth = 2;
		c.weighty=0;
		getContentPane().add(buttonPane, c);
		this.setSize(700, 500);
		this.setLocationRelativeTo(parent);
		setVisible(true);
		
		JViewport jv = scrollPane.getViewport();  
		jv.setViewPosition(new Point(0,0)); 
		infopane.setCaretPosition(0);
	}

	private void createContent() {
		infopane = new HTMLPaneWithButtons();
		try {
			infopane.appendHTML(method.getInfo());
		} catch (Exception e) {
			infopane.setText("An error occured and no info is available...");
			ErrorHandler.getInstance().makeErrDialog("An error occured and no info is available...", e, false);
		}
	}
	
	private JPanel getArgumentPanel() {
		JPanel panel = new JPanel();
		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.HORIZONTAL;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.insets = new Insets(5, 5, 5, 5); 
		c.weightx=1; c.weighty=0;
		c.anchor = GridBagConstraints.PAGE_START;
		panel.setLayout(new GridBagLayout());
		
		if (method.getErrorControl()!=null) {
			panel.add(new JLabel("Error Control: "+method.getErrorControl()), c);
			c.gridy++;
		}
		
		for (Parameter p : method.getParameters()) {
			if (p.isAdditional()) {
				if (p.isOptional()) {
					JCheckBox jch = new JCheckBox("Enable this parameter:", false);
					panel.add(jch, c);
					c.gridwidth=2;
					htC.put(p.getName(), jch);
					c.gridwidth=1;
					c.gridy++;
				}
				ParameterWidget widget = null;
				if (p.getType().equals("numeric")) {
					panel.add(new JLabel(p.getLabel()), c);
					c.gridx++;
					widget = new NumberWidget(p.getName());
					panel.add((Component) widget, c);
					ht.put(p.getName(), widget);
					if (p.getDefaultValue() != null) {
						((NumberWidget)widget).setText(""+p.getDefaultValue().asRNumeric().getData()[0]);
					}
					c.gridx=0;c.gridy++;
				} else if (p.getType().equals("integer")) {
					panel.add(new JLabel(p.getLabel()), c);
					c.gridx++;
					widget = new IntegerWidget(p.getName());
					panel.add((Component) widget, c);					
					if (p.getDefaultValue() != null) {
						((IntegerWidget)widget).setText(""+p.getDefaultValue().asRInteger().getData()[0]);
					}					
				} else if (p.getType().equals("character")) {
					panel.add(new JLabel(p.getLabel()), c);
					c.gridx++;
					if (p.getChoices()!=null) {
						widget = new CharacterChoiceWidget(p.getName(), p.getChoices());
					} else if (p.getFromR()!=null) {
						List<RLegalName> v = new ArrayList<RLegalName>();
				        v.addAll(MuTossControl.getObj().getData().asRDataFrame().getFactorVars());
						widget = new VarSelectWidget(p.getName(), v);
					} else {
						// TODO Typ in String (for title or something)
					}
					panel.add((Component) widget, c);					
					if (p.getDefaultValue() != null) {
						//TODO set default value
					}					
				} else if (p.getType().equals("logical")) {
					panel.add(new JLabel(p.getLabel()), c);
					c.gridx++;
					widget = new BooleanWidget(p.getName());
					panel.add((Component) widget, c);					
					if (p.getDefaultValue() != null) {
						((BooleanWidget)widget).setSelected(p.getDefaultValue().asRLogical().getData()[0]);
					}					
				} else if (p.getType().equals("RObject")) {
					panel.add(new JLabel(p.getLabel()), c);
					c.gridx++;
					widget = new RObjectWidget(p.getName());
					panel.add((Component) widget, c);	
				}
				c.gridx=0;c.gridy++;
				ht.put(p.getName(), widget);
			}
		}		
		
		return panel;
	}
	
	String selected = null;

	public String getSelected() {
		return selected;
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getActionCommand().equals(HorizontalButtonPane.OK_CMD)) {
			RList argsList = MuTossControl.getR().eval("as.list(formals("+method.getFunction()+"))").asRList();
			RObj names = MuTossControl.getR().eval("names(formals("+method.getFunction()+"))");
			String[] args = names!=null?MuTossControl.getR().eval("names(formals("+method.getFunction()+"))").asRChar().getData():new String[0];
			List<Object> rargs = new Vector<Object>();
			List<String> unknownArgs = new Vector<String>();
			for (String arg : args) {
				if (arg.equals("pValues") && MuTossControl.getObj().hasPValues()) {
					rargs.add(new RNamedArgument("pValues", MuTossControl.getObj().getPValues()));
				} else if (arg.equals("alpha") && MuTossControl.getObj().hasErrorRate()) {
					rargs.add(new RNamedArgument("alpha", MuTossControl.getObj().getAlpha()));
				} else if (arg.equals("data") && MuTossControl.getObj().hasData()) {
					rargs.add(new RNamedArgument("data", MuTossControl.getObj().getData()));
				} else if (arg.equals("model") && MuTossControl.getObj().hasModel()) {
					rargs.add(new RNamedArgument("model", MuTossControl.getObj().getRModel()));
				} else if (arg.equals("hypotheses") && MuTossControl.getObj().hasModel()) {
					rargs.add(new RNamedArgument("hypotheses", MuTossControl.getObj().getHypotheses()));
				} else if (arg.equals("adjPValues") && MuTossControl.getObj().hasAdjPValues()) {
					rargs.add(new RNamedArgument("adjPValues", MuTossControl.getObj().getAdjPValues()));
				} else {
					if (ht.get(arg)!=null && (htC.get(arg)==null || htC.get(arg).isSelected())) { // This can happen, if there are further options that are not described in the MuTossMethod-Object.
						rargs.add(ht.get(arg).getParameter());
					} else {
						logger.warn("Oh no! Argument "+arg+" can not be found!");
						//ErrorHandler.getInstance().makeErrDialog("Oh no! Argument "+arg+" can not be found!");
					}
				}
			}
			MuTossControl.getR().eval("mutossGUI:::startRecording()");			
			RList output = MuTossControl.getR().call(method.getFunction(), rargs.toArray()).asRList();
			MuTossControl.getR().eval("mutossGUI:::stopRecording()");
			OutputPanel.getOutputPanel().getOutputPane().appendHeadline(method.getLabel());
			String[] rOutput = MuTossControl.getR().eval("mutossGUI:::getOutput()").asRChar().getData();
			String outputString = "<p align=\"left\"><pre>";
			for (String ro : rOutput) {
				outputString += ro + "\n";
			}
			outputString += "</pre></p>";
			OutputPanel.addOutput(outputString);	
			String[] rError = MuTossControl.getR().eval("mutossGUI:::getErrorMsg()").asRChar().getData();
			String errorString = "<p align=\"left\"><font color=#FF0000>";
			for (String ro : rError) {
				errorString += ro + "\n";
			}
			errorString += "</font></p>";
			OutputPanel.addOutput(errorString);			
			for (String name : output.getNames()) {
				if ((name).equals("adjPValues")) {
					logger.info("Method "+method.getLabel()+" is setting the adjusted p-values.");
					MuTossControl.getObj().setAdjPValues(output.get("adjPValues"), method.getLabel());
				} else if ((name).equals("rejected")) {
					logger.info("Method "+method.getLabel()+" is setting the rejected hypotheses.");
					MuTossControl.getObj().setRejected(output.get("rejected"), method.getLabel());
				} else if ((name).equals("confIntervals")) {
					logger.info("Method "+method.getLabel()+" is setting the confidence intervals.");
					MuTossControl.getObj().setConfIntervals(output.get("confIntervals"), method.getLabel());
				} else if ((name).equals("pValues")) {
					logger.info("Method "+method.getLabel()+" is setting the unadjusted p-values.");
					MuTossControl.getObj().setPValues(output.get("pValues"), method.getLabel());
				} else if ((name).equals("model")) {
					logger.info("Method "+method.getLabel()+" is setting the model.");
					MuTossControl.getObj().setModel(output.get("model"), method.getLabel());
				} else if ((name).equals("pi0")) {
					logger.info("Method "+method.getLabel()+" is setting the pi0.");
					MuTossControl.getObj().setP0(output.get("pi0"), method.getLabel());
				} else if ((name).equals("errorControl")) {
					logger.info("Method "+method.getLabel()+" is setting the errorControl.");
					MuTossControl.getObj().setErrorControl(output.get("errorControl"), method.getLabel());
				}  
			}
			dispose();
		} else if (e.getActionCommand().equals(HorizontalButtonPane.CANCEL_CMD)) {
			dispose();
		}
	}
	
}
