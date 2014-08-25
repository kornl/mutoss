package org.mutoss.gui.dialogs;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.rmi.RemoteException;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import org.af.commons.widgets.WidgetFactory;
import org.af.jhlir.backends.rengine.RMatrixDoubleREngine;
import org.af.jhlir.call.RDataFrame;
import org.af.jhlir.call.RMatrixDouble;
import org.mutoss.Model;
import org.mutoss.MuTossControl;
import org.mutoss.gui.widgets.AnalysisDialogFactory;
import org.mutoss.gui.widgets.ContrastTable;
import org.mutoss.gui.widgets.GroupingBox;
import org.mutoss.gui.widgets.LevelOrderSL;
import org.mutoss.gui.widgets.ROptionBox;
import org.rosuda.REngine.REXPDouble;

public class ContrastDialog2 extends StatMethodDialog implements ActionListener {
    
    private GroupingBox cbGroup;
    private LevelOrderSL slLevelOrder;
    //private JComboBox jbcontrol;
    private ROptionBox<String> cbType;
    private ContrastTable pTable;
    private JButton jbAdd;
    private JButton jbScaleCM;
    public JLabel jlInfo; 
    
    public ContrastDialog2(JFrame window, RDataFrame df, Model model) {
        super(window, "Contrast Building", df);       
        cbGroup = AnalysisDialogFactory.makeGroupingComboBox(df);
        slLevelOrder = AnalysisDialogFactory.makeLevelOrderSL(df, cbGroup);
        slLevelOrder.allToRight();
        
        jbAdd = new JButton("Add Contrast");
        jbAdd.addActionListener(this); 
        jbScaleCM = new JButton("Scale contrasts");
        jbScaleCM.addActionListener(this);
        jlInfo = new JLabel("Please add contrasts.");
        
        pTable = new ContrastTable(df, cbGroup, slLevelOrder, jlInfo);
        
        String[] a = {"Dunnett", "Tukey", "Sequen", "AVE", 
                "Changepoint", "Williams", "Marcus", 
                "McDermott", "UmbrellaWilliams", "GrandMean"}; // This array you are allowed to rename!
        String[] b = {"Dunnett", "Tukey", "Sequen", "AVE", 
                "Changepoint", "Williams", "Marcus", 
                "McDermott", "UmbrellaWilliams", "GrandMean"};        
        cbType = new ROptionBox<String>("type", a, b);
        
        JPanel panel = new JPanel();
        panel.add(jbAdd);
        panel.add(jbScaleCM);
        
        String[] labels = {//"Group", "Level Order", //"Control", 
        		"Type", null, "Contrasts", null};
        
        Component[] comps = {//cbGroup, slLevelOrder, //jbcontrol, 
        		cbType, panel, new JScrollPane(pTable), jlInfo};

        setContent(labels, comps);
        setModal(true);
        doTheLayout();
        pack();
        WidgetFactory.showCompleteTitle(this);
        setLocationRelativeTo(getOwner()); 
        setVisible(true);
    }
    
    protected void checkConstraints() {        
        requiresFactVar();
    }
    
    public RMatrixDouble showAndGetMatrix() {
    	setVisible(true);
    	dispose();
    	return getMatrix();
    }
    
    public RMatrixDouble getMatrix() {
		return null;    	
		//return createMatrix(pTable.getMatrix(), "m"); 
    }

    /* protected void onOk() {
        String group = cbGroup.getSelectedItem().toString();
		List<String> levelOrder = slLevelOrder.getRight();
		//String contr = jbcontrol.getSelectedItem().toString();
		String type = cbType.getSelectedObject();
		dispose();
    }*/
    
    public void actionPerformed(ActionEvent e) {
        super.actionPerformed(e);
        if (e.getSource().equals(jbAdd)) {        	
        	RMatrixDouble m = getContrastMatrix(cbGroup.getSelectedItem().toString(), cbType.getSelectedObject(), slLevelOrder.getRight());        	
        	try {
				pTable.addMatrix(m);
			} catch (RemoteException e1) {
				// TODO Auto-generated catch block
				e1.printStackTrace();
			}
        	/* List<Double> l = new Vector<Double>();        	
        	pTable.addMatrix(getAnalysis().getColumns(l, m)); */
        } else if (e.getSource().equals(jbScaleCM)) {        	
        	pTable.scale();
        }        
    }
    

	/**
	 * @param groupvarname column of dataframe for which the contrast should be generated.
	 * @param type type of contrast, i.e. one of 
	 * "Dunnett", "Tukey", "Sequen", "AVE",
	 * "Changepoint", "Williams", "Marcus",
	 * "McDermott", "UmbrellaWilliams" or "GrandMean".
	 * @param list 
	 */
	 public RMatrixDouble getContrastMatrix(String groupvarname, String type, List<String> list) {
		return new RMatrixDoubleREngine(MuTossControl.getR(), (REXPDouble) MuTossControl.getR().call("mutossGUI:::myContrMat", type, list, df, groupvarname).getWrapped());
	} 
	
	/**
	 * @param varname column of dataframe for which the contrast should be generated.
	 * @param type type of contrast, i.e. one of 
	 * "Dunnett", "Tukey", "Sequen", "AVE",
	 * "Changepoint", "Williams", "Marcus",
	 * "McDermott", "UmbrellaWilliams" or "GrandMean".
	 * @param list 
	 */
	/* public RMatrixDouble getNomContrastMatrix(String varname, String type, List<String> list) {
		RFunctionCall rfc = new RFunctionCall(getRControl(), "myContrMatNom", "mn");
		DfRefWrapper df = getControl().getDataFrame();
		rfc.addParam("df", df);
		rfc.addParam("group", varname);
		//rfc.addParam("n", RFunctionCall.Type.EXPRESSION, "table("+df+"$"+varname+")");		
		rfc.addParam("type", type);
		rfc.addParam("l", list);
		//rfc.addParam("base", base+1);
		
		return new RMatrixDouble(rfc.doIt()); 
	}*/
	
	/**
	/**
	 * @param varname column of dataframe for which the contrast should be generated.
	 * @param type type of contrast, i.e. one of 
	 * "Dunnett", "Tukey", "Sequen", "AVE",
	 * "Changepoint", "Williams", "Marcus",
	 * "McDermott", "UmbrellaWilliams" or "GrandMean".
	 * @param list 
	 */
	/* public RMatrixDouble getDenomContrastMatrix(String varname, String type, List<String> list) {
		RFunctionCall rfc = new RFunctionCall(getRControl(), "myContrMatDenom", "md");
		DfRefWrapper df = getControl().getDataFrame();
		rfc.addParam("df", df);
		rfc.addParam("group", varname);
		//rfc.addParam("n", RFunctionCall.Type.EXPRESSION, "table("+df+"$"+varname+")");		
		rfc.addParam("type", type);
		rfc.addParam("l", list);
		//rfc.addParam("base", base+1);
		
		return new RMatrixDouble(rfc.doIt());
	} */
	
	/**
	 * An empty List l will cause an error.
	 * @param l
	 * @param m
	 * @return
	 */
   /* public RMatrixDouble getColumns(List<Double> l, RMatrixDouble m) {
    	String index = null;
    	for (Double d : l) {
    		if (index==null) {
    			index = "[";
    		} else {
    			index +=", ";
    		}
    		index += d; 
    	}
    	RFunctionCall rfc = new RFunctionCall(getRControl(), m+index+"]", "msub");
    	rfc.setAddParams(false);
		//DfRefWrapper df = getControl().getDataFrame();		
		return new RMatrixDouble(rfc.doIt());    	
    } */

    
	 protected void onOk() throws RemoteException {
		 MuTossControl.getObj().setHypotheses(MuTossControl.getR().eval(pTable.getMatrix()));
		 dispose();
	 }

}

