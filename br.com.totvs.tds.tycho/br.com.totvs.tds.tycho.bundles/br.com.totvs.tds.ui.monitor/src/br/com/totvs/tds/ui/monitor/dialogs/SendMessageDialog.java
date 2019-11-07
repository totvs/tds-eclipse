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
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;

import br.com.totvs.tds.ui.monitor.MonitorUIIcons;
import br.com.totvs.tds.ui.monitor.model.IItemMonitor;
import br.com.totvs.tds.ui.monitor.model.IServerMonitor;
import br.com.totvs.tds.ui.monitor.model.IUserMonitor;

public class SendMessageDialog extends TitleAreaDialog {

	private IItemMonitor itemsMonitor[];

	private Table table_1;
	private Text txtMessage;

	private CheckboxTableViewer viewerRecipients;

	private ArrayList<IUserMonitor> recipients;

	private String textMessage;

	public SendMessageDialog(final Shell parentShell, final IItemMonitor[] itemMonitor) {
		super(parentShell);

		this.itemsMonitor = itemMonitor;
		setBlockOnOpen(true);
	}

	@Override
	protected Control createContents(final Composite parent) {
		final Control control = super.createContents(parent);

		setTitle("Envio de Mensagens");
		setTitleImage(MonitorUIIcons.getSendMessage().createImage());
		setMessage("Envia mensagens de texto curtas ao usuários selecionados.", IMessageProvider.INFORMATION);

		return control;
	}

	@Override
	protected Control createDialogArea(final Composite parent) {
		final Composite composite = (Composite) super.createDialogArea(parent);
		composite.setLayout(new GridLayout(1, false));

		final SashForm sashForm = new SashForm(composite, SWT.SMOOTH | SWT.VERTICAL);
		sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

		final Composite compositeRecipient = new Composite(sashForm, SWT.NONE);
		compositeRecipient.setLayout(new GridLayout(1, false));

		final Label lblDestinatrios = new Label(compositeRecipient, SWT.NONE);
		lblDestinatrios.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		lblDestinatrios.setText("Destinat\u00E1rios");

		viewerRecipients = CheckboxTableViewer.newCheckList(compositeRecipient, SWT.BORDER | SWT.FULL_SELECTION);
		table_1 = viewerRecipients.getTable();
		table_1.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
		viewerRecipients.setContentProvider(ArrayContentProvider.getInstance());
		viewerRecipients.setLabelProvider(getLabelProvider());
		viewerRecipients.setInput(itemsMonitor);
		viewerRecipients.setAllChecked(true);

		final Composite compositeMessage = new Composite(sashForm, SWT.NONE);
		compositeMessage.setLayout(new GridLayout(1, false));

		final Label lblMensagem = new Label(compositeMessage, SWT.NONE);
		lblMensagem.setText("Mensagem");

		txtMessage = new Text(compositeMessage, SWT.BORDER | SWT.WRAP | SWT.MULTI);
		txtMessage.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));

		sashForm.setWeights(new int[] { 1, 2 });

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

//	for (final IItemMonitor itemMonitor : itemsMonitor) {
//		if (itemMonitor instanceof IUserMonitor) {
//			if (itemsMonitor.length <= 1) {
//				names = ((IUserMonitor) itemMonitor).getUsername();
//				isSingular = true;
//			} else {
//				isSingular = false;
//				names += ((IUserMonitor) itemMonitor).getUsername() + ", "; //$NON-NLS-1$
//			}
//		} else {
//			if (itemsMonitor.length <= 1) {
//				isUser = false;
//				isSingular = true;
//				names = ((IServerMonitor) itemMonitor).getServerName();
//			} else {
//				isUser = false;
//				isSingular = false;
//				names += ((IServerMonitor) itemMonitor).getServerName() + ", "; //$NON-NLS-1$
//			}
//		}
//	}
//
//	if (names.endsWith(", ")) { //$NON-NLS-1$
//		names = names.substring(0, names.length() - 2);
//	}
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
		recipients = new ArrayList<IUserMonitor>();

		for (final Object recipient : viewerRecipients.getCheckedElements()) {
			if (recipient instanceof IUserMonitor) {
				recipients.add((IUserMonitor) recipient);
			} else {
				recipients.addAll(((IServerMonitor) recipient).getChildren());
			}
		}

		textMessage = txtMessage.getText();

		super.okPressed();
	}

	public ArrayList<IUserMonitor> getRecipients() {

		return recipients;
	}

	public String getMessageText() {

		return textMessage;
	}
}
