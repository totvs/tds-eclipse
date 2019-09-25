package br.com.totvs.tds.ui.server.status;

import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.wb.swt.ResourceManager;

import br.com.totvs.tds.server.interfaces.IOrganization;
import br.com.totvs.tds.server.interfaces.ISubsidiary;
import br.com.totvs.tds.ui.server.nl.Messages;

public class SelectOrganizationDialog extends TitleAreaDialog {

	private ComboViewer cmbCompany;
	private ComboViewer cmdSubisidiary;
	// private IAppServerInfo server;
	private List<IOrganization> organizations;
	private IOrganization organization;

	private String currentEnvironment;

	private IOrganization selectedCompany;
	private String serverName;

	private ISubsidiary subsidiary;

	private Text txtEnvironment;
	private Text txtServer;

	/**
	 * Create the dialog.
	 *
	 * @param parentShell
	 * @wbp.parser.constructor
	 */
	public SelectOrganizationDialog(Shell parentShell) {
		super(parentShell);
	}

	/**
	 * Construtor.
	 *
	 * @param shell
	 * @param server
	 * @param organizations
	 */
	public SelectOrganizationDialog(Shell shell, String serverName, String currentEnvironment,
			List<IOrganization> organizations, Object company) {
		this(shell);
		this.serverName = serverName;
		this.currentEnvironment = currentEnvironment;
		this.organizations = organizations;
		this.organization = (IOrganization) company;

	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
	 * .Shell)
	 */
	@Override
	protected void configureShell(final Shell shell) {
		super.configureShell(shell);
		shell.setText(Messages.SelectOrganizationDialog_0);
	}

	/**
	 * Create contents of the button bar.
	 *
	 * @param parent
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/**
	 * Create contents of the dialog.
	 *
	 * @param parent
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		setTitleImage(ResourceManager.getPluginImage("br.com.totvs.tds.ui.server", "icons/empfil.png")); //$NON-NLS-1$ //$NON-NLS-2$
		setMessage(Messages.SelectOrganizationDialog_dialog_description);
		setTitle(Messages.SelectOrganizationDialog_dialog_description);
		Composite area = (Composite) super.createDialogArea(parent);
		area.setLayout(new GridLayout(1, false));

		Composite composite = new Composite(area, SWT.NONE);
		composite.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		composite.setLayout(new GridLayout(2, false));

		Label lblServidor = new Label(composite, SWT.NONE);
		lblServidor.setSize(0, 0);
		lblServidor.setText(Messages.SelectOrganizationDialog_server);

		txtServer = new Text(composite, SWT.BORDER);
		txtServer.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtServer.setEnabled(false);

		Label lblAmbiente = new Label(composite, SWT.NONE);
		lblAmbiente.setText(Messages.SelectOrganizationDialog_envinroment);

		txtEnvironment = new Text(composite, SWT.BORDER);
		txtEnvironment.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtEnvironment.setEnabled(false);

		Label lblEmpresa = new Label(composite, SWT.NONE);
		lblEmpresa.setText(Messages.SelectOrganizationDialog_company);

		cmbCompany = new ComboViewer(composite, SWT.READ_ONLY);
		Combo combo = cmbCompany.getCombo();
		combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

		Label lblFilial = new Label(composite, SWT.NONE);
		lblFilial.setText(Messages.SelectOrganizationDialog_subsidiary);

		cmdSubisidiary = new ComboViewer(composite, SWT.READ_ONLY);
		Combo combo_1 = cmdSubisidiary.getCombo();
		combo_1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cmdSubisidiary.getControl().setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

		cmdSubisidiary.setContentProvider(ArrayContentProvider.getInstance());
		cmdSubisidiary.setLabelProvider(new LabelProvider() {

			@Override
			public String getText(Object element) {
				if (element instanceof ISubsidiary) {
					ISubsidiary fil = (ISubsidiary) element;
					return String.format("[%s] %s", fil.getCode(), fil.getName()); //$NON-NLS-1$
				}
				return super.getText(element);
			}
		});

		cmdSubisidiary.addSelectionChangedListener(new ISelectionChangedListener() {

			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();

				if (selection.size() > 0) {

					if (selection.getFirstElement() instanceof ISubsidiary) {
						subsidiary = (ISubsidiary) selection.getFirstElement();
					}

					validateSelection(organization, subsidiary);
				}

			}
		});
		cmbCompany.getControl().setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cmbCompany.setContentProvider(ArrayContentProvider.getInstance());
		cmbCompany.setLabelProvider(new LabelProvider() {

			@Override
			public String getText(Object element) {
				if (element instanceof IOrganization) {
					IOrganization comp = (IOrganization) element;
					return String.format("[%s] %s", comp.getCode(), comp.getName()); //$NON-NLS-1$
				}
				return super.getText(element);
			}
		});

		cmbCompany.setInput(organizations);
		cmbCompany.insert("--", 0); //$NON-NLS-1$

		cmbCompany.addSelectionChangedListener(new ISelectionChangedListener() {

			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();

				if (selection.size() > 0) {
					if (selection.getFirstElement() instanceof IOrganization) {
						organization = (IOrganization) selection.getFirstElement();
						subsidiary = null;
						List<ISubsidiary> subsidiaries = organization.getSubsidiaries();

						cmdSubisidiary.setInput(subsidiaries);
					} else {
						organization = null;
						subsidiary = null;
						validateSelection(organization, subsidiary);
					}

				}

			}
		});

		fillData();

		return area;
	}

	/**
	 * Preenche os campos com valores atuais.
	 */
	private void fillData() {
		txtServer.setText(serverName);
		if (currentEnvironment != null) {
			txtEnvironment.setText(currentEnvironment);
		} else {
			txtEnvironment.setText(""); //$NON-NLS-1$
		}
		if (selectedCompany != null) {
			cmdSubisidiary.setSelection(new StructuredSelection(selectedCompany));
			cmbCompany.setSelection(new StructuredSelection(selectedCompany.getCurrentSubsidiary()));
		}
	}

	/**
	 * Return the initial size of the dialog.
	 */
	@Override
	protected Point getInitialSize() {
		return new Point(450, 300);
	}

	/**
	 * @return a empresa/filial selecionada.
	 */
	public IOrganization getSelection() {
		if (organization != null) {
			organization.setCurrentSubsidiary(subsidiary);
		}
		return organization;
	}

	/**
	 * Apresenta mensagem.
	 *
	 * @param msg
	 * @param b
	 */
	private void updateDialogMessage(String message, boolean enableOK) {
		getButton(IDialogConstants.OK_ID).setEnabled(enableOK);
		getShell().setText(message);
	}

	/**
	 * Valida a seleção.
	 *
	 * @param shell
	 * @param empSelect
	 * @param filSelect
	 * @return
	 */
	private boolean validateSelection(IOrganization empSelect, ISubsidiary filSelect) {
		if ((empSelect != null) && (filSelect == null)) {
			updateDialogMessage(Messages.SelectOrganizationDialog_select_subsidiary, false);
			return false;
		}

		updateDialogMessage(Messages.SelectOrganizationDialog_select_company_subsidiary, true);

		return true;
	}

}
