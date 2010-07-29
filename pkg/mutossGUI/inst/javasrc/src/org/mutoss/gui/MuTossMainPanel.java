package org.mutoss.gui;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;

import org.mutoss.Method;
import org.mutoss.MethodHandler;
import org.mutoss.MuTossControl;
import org.mutoss.MuTossObject;


public class MuTossMainPanel extends JPanel implements MouseListener, ActionListener {
	
	protected JButton jbData = new JButton("Data");
	protected JButton jbModel = new JButton("Model");
	protected JButton jbHypotheses = new JButton("Hypotheses");
	protected JButton jbPValues = new JButton("p-Values");
	protected JButton jbErrorRate = new JButton("Error Rate");	
	protected JButton jbAdjPValues = new JButton("Adjusted p-Values");
	protected JButton jbCI = new JButton("Confidence Intervals");
	protected JButton jbRejected = new JButton("Rejected");
	protected JButton jbP0 = new JButton("P0");
	
	public MuTossMainLabel mlData = new MuTossMainLabel("Please load either data or p-values!");
	public MuTossMainLabel mlModel = new MuTossMainLabel();
	public MuTossMainLabel mlHypotheses = new MuTossMainLabel();
	public MuTossMainLabel mlPValues = new MuTossMainLabel("Please load either data or p-values!");
	public MuTossMainLabel mlErrorRate = new MuTossMainLabel();
	public MuTossMainLabel mlAdjPValues = new MuTossMainLabel();
	public MuTossMainLabel mlCI = new MuTossMainLabel();
	public MuTossMainLabel mlRejected = new MuTossMainLabel();
	public MuTossMainLabel mlP0 = new MuTossMainLabel();
	
	
	public MuTossMainPanel() {
		setLayout(new GridBagLayout());

		GridBagConstraints c = new GridBagConstraints();
		
		c.fill = GridBagConstraints.BOTH;		
		c.gridx=0; c.gridy=0;
		c.gridwidth = 1; c.gridheight = 1;
		c.ipadx=5; c.ipady=5;
		c.insets = new Insets(5, 5, 5, 5); 
		c.weightx=1; c.weighty=1;
		
		add(jbData, c);
		jbData.addMouseListener(this);
		c.gridy++;
		add(jbModel, c);
		jbModel.addMouseListener(this);
		jbModel.setEnabled(false);
		c.gridy++;
		add(jbHypotheses, c);
		jbHypotheses.addMouseListener(this);
		jbHypotheses.setEnabled(false);
		c.gridy++;
		add(jbPValues, c);
		jbPValues.addMouseListener(this);
		c.gridy++;
		add(jbErrorRate, c);
		jbErrorRate.addMouseListener(this);
		jbErrorRate.setEnabled(false);
		c.gridy++;
		add(jbP0, c);
		jbP0.addMouseListener(this);
		jbP0.setEnabled(false);
		c.gridy++;
		add(jbAdjPValues, c);
		jbAdjPValues.addMouseListener(this);
		jbAdjPValues.setEnabled(false);
		c.gridy++;
		add(jbCI, c);
		jbCI.addMouseListener(this);
		jbCI.setEnabled(false);
		c.gridy++;
		add(jbRejected, c);
		jbRejected.addMouseListener(this);
		jbRejected.setEnabled(false);
		c.gridy++;	
		
		c.gridy = 0;
		c.gridx++;
		
		add(mlData, c);		
		c.gridy++;
		add(mlModel, c);		
		c.gridy++;
		add(mlHypotheses, c);		
		c.gridy++;
		add(mlPValues, c);
		c.gridy++;
		add(mlErrorRate, c);
		c.gridy++;
		add(mlP0, c);
		c.gridy++;
		add(mlAdjPValues, c);
		c.gridy++;
		add(mlCI, c);
		c.gridy++;
		add(mlRejected, c);
		c.gridy++;	
		
	}


	@Override
	public void actionPerformed(ActionEvent event) {		
		
	}

	public static MuTossObject getObj() {
		return MuTossControl.getObj();		
	}
	

	@Override
	public void mouseClicked(MouseEvent event) {
		JPopupMenu popupmenu = new JPopupMenu();
		JMenuItem item;		
		if (event.getSource()== jbData) {
			createItem(popupmenu, "Load R Object", MuTossControl.LOAD_R_DATA);
			popupmenu.addSeparator();
			createItem(popupmenu, "Info", MuTossControl.DATA_INFO, getObj().hasData());			
		} else if (event.getSource()== jbModel) {
			createItem(popupmenu, "Load R Object", MuTossControl.LOAD_R_MODEL);
			popupmenu.addSeparator();
			JMenu modelm = new JMenu("Specify Model");
			JMenu modelm2 = new JMenu("Single endpoint in k groups (k >= 3)");
			createItem(modelm2, "Linear model", MuTossControl.MODEL_LM);
			//createItem(modelm2, "All pairs test (Tukey)", MuTossControl.MODEL_TUKEY);
			//createItem(modelm2, "Comparison with one control group (Dunnett)", MuTossControl.MODEL_DUNNETT);
			//createItem(modelm2, "General linear contrast tests", MuTossControl.MODEL_CONTRASTS);
			modelm.add(modelm2);
			modelm2 = new JMenu("Multiple endpoints in k groups (k >= 1)");
			createItem(modelm2, "One-sample t-test for each marginal", MuTossControl.MODEL_ONESAMPLE_T);
			createItem(modelm2, "Paired t-test for each marginal", MuTossControl.MODEL_PAIRED_T);
			createItem(modelm2, "Two-sample t-test for each marginal", MuTossControl.MODEL_TWOSAMPLE_T);
			createItem(modelm2, "F-test for equality of means for each marginal", MuTossControl.MODEL_F);
			createItem(modelm2, "Multiple endpoints and multiple contrasts", MuTossControl.MODEL_HASLER);			
			modelm.add(modelm2);
			modelm2 = new JMenu("Multiple (linear) regression (single or multiple response(s), k >= 1 covariates)");
			createItem(modelm2, "Multiple endpoints, one covariate of interest", MuTossControl.MODEL_GILL1);
			createItem(modelm2, "Single endpoints, multiple covariate of interest", MuTossControl.MODEL_GILL2);
			modelm.add(modelm2);
			popupmenu.add(modelm);
			popupmenu.addSeparator();
			createItem(popupmenu, "Info", MuTossControl.MODEL_INFO, getObj().hasModel());						
		} else if (event.getSource()== jbHypotheses) {
			createItem(popupmenu, "Load R Object", MuTossControl.LOAD_R_HYPOTHESES);
			popupmenu.addSeparator();
			createItem(popupmenu, "All pair (Tukey)", MuTossControl.DEFINE_TUKEY, getObj().hasModel());
			createItem(popupmenu, "Comparison to Control (Dunnett)", MuTossControl.DEFINE_DUNNETT, getObj().hasModel());
			createItem(popupmenu, "Define Contrasts", MuTossControl.DEFINE_CONTRASTS, getObj().hasModel());						
		} else if (event.getSource()== jbErrorRate) {
			createItem(popupmenu, "FWER", MuTossControl.ER_FWER, true);		
			createItem(popupmenu, "FWER.weak", MuTossControl.ER_FWER_WEAK, true);
			createItem(popupmenu, "gFWER", MuTossControl.ER_GFWER, true);
			createItem(popupmenu, "FDR", MuTossControl.ER_FDR, true);	
			createItem(popupmenu, "FDX", MuTossControl.ER_FDX, true);				
			createItem(popupmenu, "perComparison", MuTossControl.DATA_INFO, true);
		} else if (event.getSource()== jbPValues) {
			createItem(popupmenu, "Load R Object", MuTossControl.LOAD_R_P_VALUES);
			popupmenu.addSeparator();
			createItem(popupmenu, "Info", MuTossControl.PVALUE_INFO, getObj().hasPValues());
			for (Method m : MethodHandler.getMethodHandler().getPValueMethods()) {
				createItem(popupmenu, m.getLabel(), m.getName());
			}
		} else if (event.getSource()== jbAdjPValues) {
			createItem(popupmenu, "Info", MuTossControl.ADJ_PVALUE_INFO, getObj().hasAdjPValues());
			popupmenu.addSeparator();			
			for (Method m : MethodHandler.getMethodHandler().getAdjustedPValueMethods()) {
				createItem(popupmenu, m.getLabel(), m.getName());
			}							
		} else if (event.getSource()== jbCI) {
			createItem(popupmenu, "Visualize", MuTossControl.CI_INFO, getObj().hasCI());
			for (Method m : MethodHandler.getMethodHandler().getCIMethods()) {
				createItem(popupmenu, m.getLabel(), m.getName());
			}
		} else if (event.getSource()== jbRejected) {
			createItem(popupmenu, "Info", MuTossControl.RELECTED_INFO, getObj().hasRejected());
			popupmenu.addSeparator();			
			for (Method m : MethodHandler.getMethodHandler().getRejectedMethods()) {
				createItem(popupmenu, m.getLabel(), m.getName());
			}								
		}
		popupmenu.show(event.getComponent(),
                event.getX(), event.getY());
	}


	private void createItem(JComponent popupmenu, String itemtitle,
			String actionCommand) {
		createItem(popupmenu, itemtitle, actionCommand, true);
	}
	
	private void createItem(JComponent popupmenu, String itemtitle,
			String actionCommand, Boolean enabled) {
		JMenuItem item = new JMenuItem(itemtitle);
		item.setActionCommand(actionCommand);
		item.setEnabled(enabled);
		item.addActionListener(MuTossControl.getInstance());
		popupmenu.add(item);
	}

	public void mouseEntered(MouseEvent arg0) {}
	public void mouseExited(MouseEvent arg0) {}
	public void mousePressed(MouseEvent arg0) {}
	public void mouseReleased(MouseEvent arg0) {}
	
	public void enableAllButtons() {
		jbData.setEnabled(true);
		jbModel.setEnabled(true);
		jbHypotheses.setEnabled(true);
		jbPValues.setEnabled(true);
		jbErrorRate.setEnabled(true);
		jbAdjPValues.setEnabled(true);	
		jbCI.setEnabled(true);
		jbRejected.setEnabled(true);
		jbP0.setEnabled(true);
	}
	
	/*public void enableButtons() {
		MuTossObject obj = MuTossControl.getObj();
		jbData.setEnabled(!(obj.hasData() || obj.hasPValues() || obj.hasAdjPValues() || obj.hasRejected()));
		jbModel.setEnabled(obj.hasData() && !(obj.hasModel() || obj.hasPValues() || obj.hasAdjPValues() || obj.hasRejected()));
		jbHypotheses.setEnabled(true);
		jbPValues.setEnabled(!(obj.hasPValues() || obj.hasAdjPValues() || obj.hasRejected()));
		jbErrorRate.setEnabled(!(obj.hasErrorRate()));
		jbAdjPValues.setEnabled((obj.hasData() || obj.hasPValues()) && !(obj.hasAdjPValues() || obj.hasRejected()));
		jbCI.setEnabled(obj.hasData());
		jbRejected.setEnabled((obj.hasData() || obj.hasPValues()) && !(obj.hasRejected()));
		jbP0.setEnabled(!obj.hasP0());
	}*/
}
