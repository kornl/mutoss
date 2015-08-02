package org.mutoss;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JOptionPane;

import org.af.commons.errorhandling.ErrorHandler;
import org.af.jhlir.backends.rengine.RCallServicesREngine;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.mutoss.gui.DebugTextConsole;
import org.mutoss.gui.JavaGDPanel;
import org.mutoss.gui.MuTossGUI;
import org.mutoss.gui.MuTossMainPanel;
import org.mutoss.gui.OutputPanel;
import org.mutoss.gui.dialogs.AlphaDialog;
import org.mutoss.gui.dialogs.ContrastDialog2;
import org.mutoss.gui.dialogs.model.OneEndpointLM;
import org.mutoss.gui.loaddata.RObjectChooser;
import org.rosuda.JRI.Rengine;
import org.rosuda.REngine.REngineException;
import org.rosuda.REngine.JRI.JRIEngine;

public class MuTossControl implements ActionListener {
	
	public static final String LOAD_R_DATA = "load r data";
	public static final String DATA_INFO = "show data info";
	public static final String LOAD_R_P_VALUES = "load r p-values";
	public static final String PVALUE_INFO = "info p-values";
	public static final String ADJ_PVALUE_INFO = "info ajusted p-values";
	public static final String RELECTED_INFO = "info rejected";
	public static final String LOAD_R_MODEL = "load r model";
	public static final String SPECIFY_MODEL = "specify model";	
	public static final String MODEL_INFO = "show model info";
	public static final String LOAD_R_HYPOTHESES = "load r hypotheses";
	public static final String DEFINE_CONTRASTS = "define contrasts";
		
	private static final Log logger = LogFactory.getLog(MuTossControl.class);
	public static final String ER_FWER = "FWER";
	public static final String ER_FWER_WEAK = "FWER_WEAK";
	public static final String ER_GFWER = "GFWER";
	public static final String ER_FDR = "FDR";
	public static final String ER_FDX = "FDX";
	public static final String MODEL_TUKEY = "model tukey";
	public static final String MODEL_DUNNETT = "model dunnett";
	public static final String MODEL_CONTRASTS = "model contrasts";
	public static final String MODEL_ONESAMPLE_T = "model onesample t";
	public static final String MODEL_PAIRED_T = "model paired t";
	public static final String MODEL_TWOSAMPLE_T = "model twosample";
	public static final String MODEL_F = "model f";
	public static final String MODEL_HASLER = "model hasler";
	public static final String MODEL_LM = "model lm";
	public static final String DEFINE_TUKEY = "Tukey";
	public static final String DEFINE_DUNNETT = "Dunnett";
	public static final String MODEL_GILL1 = "gill1";
	public static final String MODEL_GILL2 = "gill2";
	public static final String CI_INFO = "ci info";

	
	static RCallServicesREngine rcs = null;
	public static DebugTextConsole console = null;

	public static RCallServicesREngine getR() {
		if (rcs == null) {	
			if (!Rengine.versionCheck()) {
	            System.err.println("Error: API version of the Rengine class and the native binary differ.");
	            System.exit(1);
	        }
			Rengine rengine = Rengine.getMainEngine();
			if (rengine == null) {
				// Call java with VM arguments: -Declipse="true"
				if (System.getProperty("eclipse") != null) {
					console = new DebugTextConsole();
					rengine = new Rengine(new String[] {"--vanilla"}, true, console);
				} else {
					rengine = new Rengine();
				}
			}
			try {
				rcs = new RCallServicesREngine(new JRIEngine(rengine));
				if (System.getProperty("eclipse") != null) {
					rcs.eval("require(mutoss)");
					rcs.eval("require(mutossGUI)");
					rcs.eval("require(multcomp)");
					rcs.eval("require(survival)");
					rcs.eval(".setenv <- if (exists(\"Sys.setenv\")) Sys.setenv else Sys.putenv");
					rcs.eval(".setenv(\"JAVAGD_CLASS_NAME\"=\"org/mutoss/gui/JavaGD\")");
					rcs.eval("require(JavaGD)");		
					rcs.eval("pvalues<-c(rep(0.001,5),runif(20))");
					rcs.eval("data(iris)");
					rcs.eval("data(InsectSprays)");
					rcs.eval("amod <- aov(breaks ~ tension, data = warpbreaks)");			
					rcs.eval("X <- matrix(rnorm(1000), ncol=10)");
					rcs.eval("grouplabels <- rep(c(0,1),each=5)");
					//rcs.eval("mutossGUI:::myContrMat <- function(type,l,df,group) {	require(multcomp);	n <- table(df[,group])[as.numeric(factor(l,levels=levels(df[,group])))];	x <- contrMat(n=n,type=type)}");
				}
			} catch (REngineException e) {
				ErrorHandler.getInstance().makeErrDialog("Error creating RCallServicesREngine!", e);
			}
			OutputPanel.getOutputPanel().getOutputPane().appendParagraph("R Connection established");
	
		}		
		return rcs;
	}

	protected static MuTossControl control = null;
	public static MuTossObject mutossObject = null;

	public MuTossControl() {
		resetMuTossObject();
	}
	
	public static void resetMuTossObject() {
		mutossObject = new MuTossObject(".InternalMuTossGUIData");
	}
	
	public static MuTossControl getInstance() {
		if (control == null) control = new MuTossControl();
		return control;
	}

	public void actionPerformed(ActionEvent e) {
		MuTossMainPanel mp = MuTossGUI.getGUI().getMpanel();
		logger.info("Got ActionCommand: "+e.getActionCommand());
		if (e.getActionCommand().equals(LOAD_R_DATA)) {
			logger.info("Calling RObjectChooser.");
			RObjectChooser roc = new RObjectChooser(getGUI());
			if (roc.getSelected() != null) {
				getObj().setData(roc.getSelected(), "RObj: "+roc.getSelected());				
			}			
		} else if (e.getActionCommand().equals(DATA_INFO)) {
			
		} else if (e.getActionCommand().equals(LOAD_R_MODEL)) {
			logger.info("Calling RObjectChooser.");
			RObjectChooser roc = new RObjectChooser(getGUI());
			if (roc.getSelected() != null) {
				getObj().setModel(roc.getSelected());				
			}	
		} else if (e.getActionCommand().equals(SPECIFY_MODEL)) {
						
		} else if (e.getActionCommand().equals(DEFINE_TUKEY)) {
			getObj().setStringAsHypotheses(DEFINE_TUKEY);			
		} else if (e.getActionCommand().equals(DEFINE_DUNNETT)) {
			getObj().setStringAsHypotheses(DEFINE_DUNNETT);		
		} else if (e.getActionCommand().equals(MODEL_INFO)) {
			
		} else if (e.getActionCommand().equals(MODEL_LM)) {
			new OneEndpointLM(MuTossGUI.getGUI(), getObj().getData().asRDataFrame()); // TODO Catch java.lang.ClassCastException
		} else if (e.getActionCommand().equals(DEFINE_CONTRASTS)) {
			new ContrastDialog2(MuTossGUI.getGUI(), getObj().getData().asRDataFrame(), getObj().getModel());  // TODO Catch java.lang.ClassCastException
		} else if (e.getActionCommand().equals(LOAD_R_P_VALUES)) {
			logger.info("Calling RObjectChooser.");
			RObjectChooser roc = new RObjectChooser(MuTossGUI.getGUI());
			if (roc.getSelected() != null) {
				getObj().setPValues(roc.getSelected(), "RObj: "+roc.getSelected());
			}						
		} else if (e.getActionCommand().equals(PVALUE_INFO)) {
			getR().eval("JavaGD()");
			getR().eval("pValuesPlot("+getObj().getObjName()+"@pValues)");
			getGUI().getTabbedPane().addTab("P-Value Plot", new JavaGDPanel());
		} else if (e.getActionCommand().equals(ADJ_PVALUE_INFO)) {
			getR().eval("JavaGD()");
			getR().eval("adjPValuesPlot("+getObj().getObjName()+"@adjPValues)");
			getGUI().getTabbedPane().addTab("Adjusted P-Value Plot", new JavaGDPanel());
		} else if (e.getActionCommand().equals(CI_INFO)) {
			getR().eval("JavaGD()");
			getR().eval("mutoss.plotCI("+getObj().getObjName()+"@confIntervals)");
			getGUI().getTabbedPane().addTab("Confidence Interval Plot", new JavaGDPanel());
		} else if (e.getActionCommand().equals(ER_FWER) || e.getActionCommand().equals(ER_GFWER) || e.getActionCommand().equals(ER_FDR)) {
			new AlphaDialog(MuTossGUI.getGUI(), e.getActionCommand());
		} else if (e.getActionCommand().equals(MODEL_ONESAMPLE_T)) {
			MethodHandler.getMethodHandler().apply("mutoss.onesamp.model");
			
		} else if (e.getActionCommand().equals(MODEL_PAIRED_T)) {
					MethodHandler.getMethodHandler().apply("mutoss.paired.model");
		} else if (e.getActionCommand().equals(MODEL_TWOSAMPLE_T)) {
			MethodHandler.getMethodHandler().apply("mutoss.twosamp.model");
		} else if (e.getActionCommand().equals(MODEL_F)) {
			MethodHandler.getMethodHandler().apply("mutoss.ftest.model");
		} else if (e.getActionCommand().equals(MODEL_HASLER) || 
				e.getActionCommand().equals(MODEL_GILL1) || 
				e.getActionCommand().equals(MODEL_GILL2)) {
			JOptionPane.showMessageDialog(MuTossGUI.getGUI(), "Not yet implemented!", "Not yet implemented!", JOptionPane.INFORMATION_MESSAGE);
			//MethodHandler.getMethodHandler().apply("mutoss.ftest.model");
		} else if (e.getActionCommand().equals(RELECTED_INFO)) {
			getGUI().getTabbedPane().setSelectedIndex(0);
			MuTossControl.startRecording();		
			MuTossControl.getR().call("mutossGUI:::showRejected", getObj().getObjName());
			MuTossControl.stopRecording("Rejected");						
		} else { // We want to apply a function:
			MethodHandler.getMethodHandler().apply(e.getActionCommand());
		} 
		mp.enableAllButtons();
	}
	
	private static MuTossGUI getGUI() {
		return MuTossGUI.getGUI();
	}

	public static MuTossObject getObj() {
		return mutossObject;		
	}
	
    public static void exit(boolean closeGUI) {
    	if (closeGUI) { MuTossGUI.exit(); }
    	OutputPanel.close();
    	mutossObject = null;
    	control = null;
    	//System.exit(0);
	}
    
    public static void startRecording() {
    	MuTossControl.getR().eval("mutossGUI:::startRecording()");
    }
    
    
    public static void stopRecording(String header) {
    	MuTossControl.getR().eval("mutossGUI:::stopRecording()");
		OutputPanel.getOutputPanel().getOutputPane().appendHeadline(header);
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
    }
	
}
