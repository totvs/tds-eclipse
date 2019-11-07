package br.com.totvs.tds.ui.monitor.dialogs;

import java.util.ArrayList;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;

import br.com.totvs.tds.ui.monitor.MonitorUIIcons;
import br.com.totvs.tds.ui.monitor.model.IItemMonitor;
import br.com.totvs.tds.ui.monitor.model.IServerMonitor;
import br.com.totvs.tds.ui.monitor.model.IUserMonitor;

public class DisconnectUserDialog extends TitleAreaDialog {

	private IItemMonitor itemsMonitor[];

	private Table table_1;

	private CheckboxTableViewer viewerRecipients;

	private ArrayList<IUserMonitor> users;

	private Button btnImmediately;

	private Button btnNotify;

	private boolean notify;

	private boolean immediately;

	public DisconnectUserDialog(final Shell parentShell, final IItemMonitor[] itemMonitor) {
		super(parentShell);

		this.itemsMonitor = itemMonitor;
		setBlockOnOpen(true);
	}

	@Override
	protected Control createContents(final Composite parent) {
		final Control control = super.createContents(parent);

		setTitle("Desconexão de usuários");
		setTitleImage(MonitorUIIcons.getDisconnectUser().createImage());
		setMessage("Desconecta usuários selecionados.", IMessageProvider.INFORMATION);

		return control;
	}

	@Override
	protected Control createDialogArea(final Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		composite.setLayout(new GridLayout(1, false));

		final Composite compositeUsers = new Composite(composite, SWT.NONE);
		compositeUsers.setLayout(new GridLayout(2, false));

		final Label lblDestinatrios = new Label(compositeUsers, SWT.NONE);
		lblDestinatrios.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 2, 1));
		lblDestinatrios.setText("Usu\u00E1rios");

		viewerRecipients = CheckboxTableViewer.newCheckList(compositeUsers, SWT.BORDER | SWT.FULL_SELECTION);
		table_1 = viewerRecipients.getTable();
		table_1.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));
		viewerRecipients.setContentProvider(ArrayContentProvider.getInstance());
		viewerRecipients.setLabelProvider(getLabelProvider());
		viewerRecipients.setInput(itemsMonitor);
		viewerRecipients.setAllChecked(true);

		btnImmediately = new Button(compositeUsers, SWT.CHECK);
		btnImmediately.setText("Imediatamente");
		btnImmediately.setSelection(true);

		btnNotify = new Button(compositeUsers, SWT.CHECK);
		btnNotify.setText("Notificar antes");
		btnNotify.setEnabled(false);

		return composite;
	}

	private IBaseLabelProvider getLabelProvider() {
		return new ITableLabelProvider() {

			@Override
			public void removeListener(final ILabelProviderListener listener) {
				// TODO Auto-generated method stub

			}

			@Override
			public boolean isLabelProperty(final Object element, final String property) {
				// TODO Auto-generated method stub
				return false;
			}

			@Override
			public void dispose() {
				// TODO Auto-generated method stub

			}

			@Override
			public void addListener(final ILabelProviderListener listener) {
				// TODO Auto-generated method stub

			}

			@Override
			public String getColumnText(final Object element, final int columnIndex) {
				String result = null;

				if (columnIndex == 0) {
					if (element instanceof IServerMonitor) {
						final IServerMonitor serverMonitor = ((IServerMonitor) element);
						result = String.format("%s (%d usuários)", serverMonitor.getServerName(),
								serverMonitor.getChildren().size());
					} else {
						final IUserMonitor userMonitor = ((IUserMonitor) element);
						result = userMonitor.getUsername();
					}
				}

				return result;
			}

			@Override
			public Image getColumnImage(final Object element, final int columnIndex) {
				Image result = null;

				if (columnIndex == 0) {
					if (element instanceof IServerMonitor) {
						result = MonitorUIIcons.getServer().createImage();
					} else {
						result = MonitorUIIcons.getUser().createImage();
					}
				}

				return result;
			}
		};

	}

	@Override
	protected void createButtonsForButtonBar(final Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	@Override
	protected void configureShell(final Shell newShell) {
		super.configureShell(newShell);

		newShell.setText("Envio de Mensagens");
	}

	@Override
	protected void okPressed() {
		users = new ArrayList<IUserMonitor>();

		for (final Object recipient : viewerRecipients.getCheckedElements()) {
			if (recipient instanceof IUserMonitor) {
				users.add((IUserMonitor) recipient);
			} else {
				users.addAll(((IServerMonitor) recipient).getChildren());
			}
		}

		notify = btnNotify.getSelection();
		immediately = btnImmediately.getSelection();

		super.okPressed();

	}

	public ArrayList<IUserMonitor> getRecipients() {

		return users;
	}

	public boolean isNotifyBefore() {

		return notify;
	}

	public boolean isImmediately() {

		return immediately;
	}

}
