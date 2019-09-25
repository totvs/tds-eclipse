package br.com.totvs.tds.ui.dialog;

import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

public class DualSelectorDialog extends TitleAreaDialog {

	private DualSelector dualSelector;
	private List<DualListItem> items;
	private String titleSource;
	private String titleTarget;
	private String message;
	private String title;
	private Image titleImage;
	private boolean showOrder;

	/**
	 * Create the dialog.
	 * @param parentShell
	 * @param titleImage 
	 */
	public DualSelectorDialog(Shell parentShell, String title, String message, Image titleImage) {
		super(parentShell);
		
		this.title = title;
		this.message = message;
		this.titleImage = titleImage;
		
		setBlockOnOpen(true);
	}

	/**
	 * Create contents of the dialog.
	 * @param parent
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		Composite area = (Composite) super.createDialogArea(parent);
		Composite container = new Composite(area, SWT.NONE);
		container.setLayout(new FillLayout(SWT.HORIZONTAL));
		container.setLayoutData(new GridData(GridData.FILL_BOTH));
		
		dualSelector = new DualSelector(container, SWT.NONE);
		dualSelector.setTitleSource(getTitleSource());
		dualSelector.setTitleTarget(getTitleTarget());
		dualSelector.setItems(getItems());
		dualSelector.setShowOrder(isShowOrder());
		
		setTitle(title);
		setMessage(message);
		setTitleImage(titleImage);
		
		return area;
	}

	/**
	 * Create contents of the button bar.
	 * @param parent
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/**
	 * Return the initial size of the dialog.
	 */
	@Override
	protected Point getInitialSize() {
		return new Point(543, 364);
	}

	/**
	 * @param items the items to set
	 */
	public void setItems(List<DualListItem> items) {
		this.items = items;
	}

	public List<DualListItem> getItems() {
		
		return this.items;
	}

	/**
	 * @return the titleSource
	 */
	public String getTitleSource() {
		return titleSource;
	}

	/**
	 * @param titleSource the titleSource to set
	 */
	public void setTitleSource(String titleSource) {
		this.titleSource = titleSource;
	}

	/**
	 * @return the titleTarget
	 */
	public String getTitleTarget() {
		return titleTarget;
	}

	/**
	 * @param titleTarget the titleTarget to set
	 */
	public void setTitleTarget(String titleTarget) {
		this.titleTarget = titleTarget;
	}

	/**
	 * @return the showOrder
	 */
	public boolean isShowOrder() {
		return showOrder;
	}

	/**
	 * @param showOrder the showOrder to set
	 */
	public void setShowOrder(boolean showOrder) {
		this.showOrder = showOrder;
	}
}
