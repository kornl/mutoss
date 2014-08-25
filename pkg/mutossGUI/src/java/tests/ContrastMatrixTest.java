package tests;

import java.util.List;
import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JLabel;

import org.af.commons.Localizer;
import org.af.jhlir.backends.rengine.RCallServicesREngine;
import org.af.jhlir.call.RDataFrame;
import org.af.jhlir.call.RIllegalNameException;
import org.af.jhlir.call.RLegalName;
import org.mutoss.gui.DebugTextConsole;
import org.mutoss.gui.widgets.GroupingBox;
import org.mutoss.gui.widgets.LevelOrderSL;
import org.rosuda.JRI.Rengine;
import org.rosuda.REngine.REngineException;
import org.rosuda.REngine.JRI.JRIEngine;

public class ContrastMatrixTest {
	
	static RCallServicesREngine rcs = null;
	static DebugTextConsole console = null;
	
	/**
	 * @param args
	 * @throws REngineException 
	 * @throws RIllegalNameException 
	 */
	public static void main(String[] args) throws REngineException, RIllegalNameException {
		Localizer.getInstance().addResourceBundle("org.mutoss.gui.widgets.ResourceBundle");
		Rengine rengine = new Rengine(new String[] {"--vanilla"}, true, console);
		rcs = new RCallServicesREngine(new JRIEngine(rengine));
		rcs.eval("require(mutoss)");
		rcs.eval(".setenv <- if (exists(\"Sys.setenv\")) Sys.setenv else Sys.putenv");
		rcs.eval(".setenv(\"JAVAGD_CLASS_NAME\"=\"org/mutoss/gui/JavaGD\")");
		rcs.eval("require(JavaGD)");					
		rcs.eval("pvalues<-c(rep(0.0001,5),runif(995))");
		rcs.eval("data(iris)");
		rcs.eval("mutossGUI:::myContrMat <- function(type,l,df,group) {	require(multcomp);	n <- table(df[,group])[as.numeric(factor(l,levels=levels(df[,group])))];	x <- contrMat(n=n,type=type)}");
		JFrame frame = new JFrame();		
		
		List<RLegalName> list = new Vector<RLegalName>();
		list.add(new RLegalName("a"));
		RDataFrame df = rcs.eval("iris").asRDataFrame().asRDataFrame();		
		GroupingBox cbGroup = new GroupingBox(list);
		LevelOrderSL slLevelOrder = new LevelOrderSL();
		JLabel jlInfo = new JLabel("Info");
		//new GaussianKTD(new JFrame(), df);
		//new ContrastDialog(null, cbGroup, slLevelOrder);
	}

}
