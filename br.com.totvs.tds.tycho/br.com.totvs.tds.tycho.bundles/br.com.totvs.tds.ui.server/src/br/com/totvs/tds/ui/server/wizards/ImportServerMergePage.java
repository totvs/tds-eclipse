package br.com.totvs.tds.ui.server.wizards;

import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.List;

import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.vo.ServerImporExportAttributesVO;

/**
 * Confirmação do merge.
 *
 * @author acandido
 */
public class ImportServerMergePage extends WizardPage {

	private ListViewer listViewer;
	private final ServerImporExportAttributesVO serversAttributes;

	protected ImportServerMergePage(ServerImporExportAttributesVO impAttributes) {
		super(Messages.ImportServerMergePage_import_merge_page_title);
		this.serversAttributes = impAttributes;
		setDescription(Messages.ImportServerMergePage_server_dpl_warning);
	}

	@Override
	public void createControl(Composite parent) {
		setTitle(getWizard().getWindowTitle());

		Composite container = new Composite(parent, SWT.NULL);
		setControl(container);
		container.setLayout(new GridLayout(2, true));

		final Button rdoKeepServers = new Button(container, SWT.RADIO);
		rdoKeepServers.setText(Messages.ImportServerMergePage_keep_servers);
		rdoKeepServers.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				if (rdoKeepServers.getSelection() == true) {
					serversAttributes.setServerMergeOption(ServerImporExportAttributesVO.ServerMergeOption.KEEP);
					updateStatus(null);
				}
			}
		});

		final Button rdoSubstituteServers = new Button(container, SWT.RADIO);
		rdoSubstituteServers.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(SelectionEvent e) {
			}

			@Override
			public void widgetSelected(SelectionEvent e) {
				if (rdoSubstituteServers.getSelection() == true) {
					serversAttributes.setServerMergeOption(ServerImporExportAttributesVO.ServerMergeOption.SUBSTITUTE);
					updateStatus(null);
				}
			}
		});
		rdoSubstituteServers.setText(Messages.ImportServerMergePage_overwrite_servers);

		listViewer = new ListViewer(container, SWT.BORDER | SWT.V_SCROLL);
		List list = listViewer.getList();
		GridData gd_list = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);

		gd_list.heightHint = 246;
		gd_list.widthHint = 457;
		list.setLayoutData(gd_list);

	}

	public void loadRepeatedServerNames() {
		listViewer.getList().removeAll();
		for (String name : serversAttributes.getRepeatedServerNames()) {
			listViewer.getList().add(name);
		}
	}

	private void updateStatus(String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}
}
