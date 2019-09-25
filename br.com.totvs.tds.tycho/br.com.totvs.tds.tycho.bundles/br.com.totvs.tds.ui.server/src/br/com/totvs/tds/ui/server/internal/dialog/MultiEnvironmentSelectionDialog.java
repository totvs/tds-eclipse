package br.com.totvs.tds.ui.server.internal.dialog;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.HelpEvent;
import org.eclipse.swt.events.HelpListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TableItem;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * diálogo para seleção de M�ltiplos Ambientes.
 *
 * @author leo.watanabe
 *
 */
public class MultiEnvironmentSelectionDialog extends TitleAreaDialog {

	/**
	 * Implementa um label e content provider para uso interno.
	 *
	 * @author leo.watanabe
	 *
	 */
	private class AllInOneProvider extends LabelProvider implements IStructuredContentProvider {

		@Override
		public Object[] getElements(final Object inputElement) {
			return environments.toArray(new IItemInfo[environments.size()]);
		}

		@Override
		public String getText(final Object element) {
			if (element instanceof IItemInfo) {
				return ((IItemInfo) element).getName();
			}
			return ""; //$NON-NLS-1$
		}

		@Override
		public void inputChanged(final Viewer viewer, final Object oldInput, final Object newInput) {
		}

	}

	private List<IEnvironmentInfo> environments;

	private List<String> selectedEnvironments;

	private CheckboxTableViewer viewer;

	public MultiEnvironmentSelectionDialog(final Shell parentShell, final IAppServerInfo server) {
		super(parentShell);
		this.environments = server.getEnvironments();
		this.selectedEnvironments = new ArrayList<>(server.getMultiEnvironmentSelection());
	}

	@Override
	protected void configureShell(final Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(Messages.MultiEnvironmentSelectionDialog_selecting_multiple_environments);
		newShell.addHelpListener(new HelpListener() {
			@Override
			public void helpRequested(final HelpEvent e) {
				// TODO
				// http://www.eclipse.org/articles/article.php?file=Article-AddingHelpToRCP/index.html
				System.out.println("TODO"); //$NON-NLS-1$
			}
		});
	}

	@Override
	public void create() {
		super.create();
		setTitle(Messages.MultiEnvironmentSelectionDialog_selecting_multiple_environments);
		setMessage(Messages.MultiEnvironmentSelectionDialog_select_environment);
	}

	@Override
	protected Control createDialogArea(final Composite parent) {
		Composite composite = new Composite(parent, SWT.NONE);
		GridLayout layout = new GridLayout();
		layout.marginHeight = 10;
		layout.marginWidth = 10;
		layout.verticalSpacing = 10;
		layout.horizontalSpacing = 10;
		composite.setLayout(layout);
		composite.setLayoutData(new GridData(GridData.FILL_BOTH));
		composite.setFont(parent.getFont());
		// Label
		Label label = new Label(composite, SWT.NONE);
		label.setText(Messages.MultiEnvironmentSelectionDialog_environments);
		// Viewer
		viewer = CheckboxTableViewer.newCheckList(composite, SWT.BORDER);
		viewer.getTable().setLayoutData(new GridData(GridData.FILL_BOTH));
		// assinala os label e content providers e o input na viewer.
		AllInOneProvider allInOneProvider = new AllInOneProvider();
		viewer.setLabelProvider(allInOneProvider);
		viewer.setContentProvider(allInOneProvider);
		viewer.setInput(environments);
		// Build the separator line
		Label titleBarSeparator = new Label(composite, SWT.HORIZONTAL | SWT.SEPARATOR);
		titleBarSeparator.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
		//
		updateSelection();
		//
		return composite;
	}

	@Override
	protected Point getInitialSize() {
		return new Point(400, 375);
	}

	/**
	 * Obt�m os ambientes selecionados para M�ltiplos Ambientes.
	 *
	 * @return
	 */
	public List<String> getMultiEnvironmentSelection() {
		return selectedEnvironments;
	}

	@Override
	protected boolean isResizable() {
		return true;
	}

	@Override
	protected void okPressed() {
		saveInput();
		super.okPressed();
	}

	private void saveInput() {
		selectedEnvironments.clear();
		for (Object iItemInfo : viewer.getCheckedElements()) {
			selectedEnvironments.add(((IItemInfo) iItemInfo).getName());
		}
	}

	private void updateSelection() {
		TableItem[] items = viewer.getTable().getItems();
		for (TableItem tableItem : items) {
			if (selectedEnvironments.contains(tableItem.getText())) {
				tableItem.setChecked(true);
			}
		}
	}

}
