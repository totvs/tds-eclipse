package br.com.totvs.tds.ui.server.wizards.patch;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.jobs.BuildPatchAttributes;
import br.com.totvs.tds.server.jobs.BuildPatchProcessType;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.ServerUIUtil;
import br.com.totvs.tds.ui.server.internal.provider.TreeStringContentProvider;
import br.com.totvs.tds.ui.server.wizards.BuildPatchWizard;

/**
 * Pagina de seleção de servidor, ambiente, tipo de processo, etc.
 */
public class PatchSelectServerPage extends WizardPage {

	private static final String LAST_PATCH_FILE = "lastPatchFile"; //$NON-NLS-1$

	private Combo cmbServer;
	private Combo cmbProcesso;
	private final BuildPatchAttributes buildAttributes;
	private Text txtPackagePath;
	private Combo cmbWhere;
	private Button btnPathFile;
	private Combo cmbPatchType;
	private Text txtFilename;
	private Button overwrite;
	private Label lblNewLabel1;
	private Label lblOverwrite;
	private CheckboxTreeViewer treeViewerEnvironments;

	private Map<String, IServerInfo> serverMap = new HashMap<String, IServerInfo>();

	/**
	 * Create the wizard.
	 *
	 * @param attributes
	 */
	public PatchSelectServerPage(final BuildPatchAttributes attributes) {
		super("patchSelectServerPage"); //$NON-NLS-1$

		setImageDescriptor(ServerUIIcons.getBuildPatch());
		setTitle(Messages.PatchSelectServerPage_Patch_generation);
		setDescription(Messages.PatchSelectServerPage_This_wizard_assist_you_build_patch);

		this.buildAttributes = attributes;
	}

	private void addEnvironmentToCheckedList(final String environment, final List<String> checkedEnvs,
			final List<String> envListFromServer) {
		if (!checkedEnvs.contains(environment) && envListFromServer.contains(environment)) {
			checkedEnvs.add(environment);
		}
	}

	private void addMultiEnvironments(final List<String> checkedEnvs, final List<String> multiEnv,
			final List<String> envListFromServer) {
		if (multiEnv != null) {
			for (String env : multiEnv) {
				addEnvironmentToCheckedList(env.toUpperCase(), checkedEnvs, envListFromServer);
			}
		}
	}

	private String checkForErrorInPage(final boolean hasEnvironmentSelected,
			final boolean hasMultiEnvironmentSelected) {
		String errorMessage = null;
		final String requiredField = Messages.PatchSelectServerPage_Attribute_requited;
		//
		if (cmbServer.getItems().length == 0) {
			errorMessage = Messages.PatchSelectServerPage_Server_not_connected;
		} else if (buildAttributes.getServer() == null) {
			errorMessage = String.format(requiredField, Messages.PatchSelectServerPage_Server);
		} else if (!hasEnvironmentSelected) {
			errorMessage = String.format(requiredField, Messages.PatchSelectServerPage_Environment);
		} else if (buildAttributes.getProcesso() == BuildPatchProcessType.UNDEFINED) {
			errorMessage = Messages.PatchSelectServerPage_Select_generation_process;
		} else if (buildAttributes.getProcesso().equals(BuildPatchProcessType.BY_RPO) && hasMultiEnvironmentSelected) {
			errorMessage = Messages.PatchSelectServerPage_Selected_process_not_allow_more_one_environment;
		} else if (buildAttributes.getPatchFilePath().isEmpty()) {
			errorMessage = String.format(requiredField, Messages.PatchSelectServerPage_SaveTo);
		} else if (buildAttributes.isLocal()) {
			errorMessage = checkForErrorOfLocalProcess();
		}
		//
		return errorMessage;
	}

	private String checkForErrorOfLocalProcess() {
		String errorMessage = null;

		// armazena o ultimo patchFile valido para recuperar na proxima execucao
		ServerUIActivator.getDefault().getPreferenceStore().setValue(LAST_PATCH_FILE,
				buildAttributes.getPatchFilePath());

		// define extensao do arquivo padrao
		String type = "ptm"; //$NON-NLS-1$
		if (cmbPatchType != null) {
			type = cmbPatchType.getItem(cmbPatchType.getSelectionIndex()).toString();
		}

		try {
			File file = new File(buildAttributes.getPatchFilePath(), buildAttributes.getFilename() + "." + type); //$NON-NLS-1$

			if (!file.getParentFile().exists()) {
				errorMessage = Messages.PatchSelectServerPage_File_path_invalid_nonexistent;
			} else if (file.exists() && !buildAttributes.isOverwrite()) {
				overwriteSectionEnable(true);
				overwriteSelection(false);
				errorMessage = String.format(Messages.PatchSelectServerPage_File_already_exists, file.getName());
			} else if (file.exists() && buildAttributes.isOverwrite()) {
				setMessage(String.format(Messages.PatchSelectServerPage_File_will_overwritten, file.getName()),
						WARNING);
			} else {
				if (buildAttributes.getEnvironmentsList().size() > 1) {
					setMessage(Messages.PatchSelectServerPage_More_one_selected_environment_files_overwritten, WARNING);
				} else {
					setMessage(null, WARNING);
				}
				overwriteSectionEnable(false);
				overwriteSelection(false);
			}
		} catch (final Exception e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
			errorMessage = e.getMessage();
		}

		return errorMessage;
	}

	private void cleanupCheckedEnvironments(final List<String> envListFromServer, final List<String> checkedEnvs) {
		List<String> tmpList = new ArrayList<String>(checkedEnvs);

		for (String checkedEnv : tmpList) {
			if (!envListFromServer.contains(checkedEnv)) {
				checkedEnvs.remove(checkedEnv);
			}
		}
	}

	/**
	 * Create contents of the wizard.
	 *
	 * @param parent
	 */
	@Override
	public void createControl(final Composite parent) {
		final Composite container = new Composite(parent, SWT.NO_REDRAW_RESIZE);

		// shell = parent.getShell();
		setControl(container);
		container.setLayout(new GridLayout(3, false));

		final Label lblServer = new Label(container, SWT.NONE);
		lblServer.setText(Messages.PatchSelectServerPage_Server);

		cmbServer = new Combo(container, SWT.READ_ONLY);
		cmbServer.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cmbServer.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				try {
					IServerManager serverManager = ServerActivator.getDefault().getServerManager();
					final IAppServerInfo server = (IAppServerInfo) serverManager.getServer(cmbServer.getText());

					buildAttributes.setServer(server);
					refreshEnvironmentsCombo(loadEnvironments(), buildAttributes.getEnvironmentsList());
				} catch (final Exception e1) {
					e1.printStackTrace();
				}
				dialogChanged();
			}
		});
		new Label(container, SWT.NONE);

		final Label lblAmbiente = new Label(container, SWT.NONE);
		lblAmbiente.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 1, 1));
		lblAmbiente.setText(Messages.PatchSelectServerPage_Environments);

		treeViewerEnvironments = new CheckboxTreeViewer(container, SWT.BORDER);
		final Tree treeEnvironments = treeViewerEnvironments.getTree();
		treeEnvironments.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		treeViewerEnvironments.setContentProvider(new TreeStringContentProvider());
		treeViewerEnvironments.setLabelProvider(new LabelProvider());
		treeViewerEnvironments.setInput(new ArrayList<String>());
		treeViewerEnvironments.addCheckStateListener(new ICheckStateListener() {

			@Override
			public void checkStateChanged(final CheckStateChangedEvent event) {
				final Object element = event.getElement();
				final boolean checked = event.getChecked();
				processEnvironmentCheck(element, checked);
				dialogChanged();
			}
		});

		new Label(container, SWT.NONE);

		final Label lblProcess = new Label(container, SWT.NONE);
		lblProcess.setText(Messages.PatchSelectServerPage_Process);

		cmbProcesso = new Combo(container, SWT.READ_ONLY);
		final String[] items = new String[BuildPatchProcessType.values().length - 1];
		for (final BuildPatchProcessType tipo : BuildPatchProcessType.values()) {
			if (!BuildPatchProcessType.UNDEFINED.equals(tipo)) {
				items[tipo.getCode()] = tipo.getLabel();
			}
		}
		cmbProcesso.setItems(items);
		cmbProcesso.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cmbProcesso.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final BuildPatchProcessType[] tipos = BuildPatchProcessType.values();
				buildAttributes.setProcesso(tipos[cmbProcesso.getSelectionIndex() + 1]);
				dialogChanged();
			}
		});
		new Label(container, SWT.NONE);

		final Label lblPacote = new Label(container, SWT.NONE);
		lblPacote.setText(Messages.PatchSelectServerPage_SaveTo);

		final Composite composite1 = new Composite(container, SWT.NONE);
		final GridLayout gl_composite1 = new GridLayout(2, false);
		gl_composite1.verticalSpacing = 0;
		gl_composite1.marginWidth = 0;
		gl_composite1.marginHeight = 0;
		composite1.setLayout(gl_composite1);
		composite1.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cmbWhere = new Combo(composite1, SWT.READ_ONLY);
		cmbWhere.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				buildAttributes.setLocal(cmbWhere.getSelectionIndex() == 0);
				txtPackagePath.setText(""); //$NON-NLS-1$
				txtFilename.setText(""); //$NON-NLS-1$
				overwriteSectionEnable(false);
				overwriteSelection(false);
				dialogChanged();
			}
		});
		cmbWhere.setItems(new String[] { Messages.PatchSelectServerPage_Local, Messages.PatchSelectServerPage_Remote });

		txtPackagePath = new Text(composite1, SWT.BORDER);
		txtPackagePath.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		txtPackagePath.setText(ServerUIActivator.getDefault().getPreferenceStore().getString(LAST_PATCH_FILE)); // $NON-NLS-1$
		txtPackagePath.setEditable(false);
		txtPackagePath.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				buildAttributes.setPatchFilePath(txtPackagePath.getText().trim());
				dialogChanged();
			}
		});

		btnPathFile = new Button(container, SWT.NONE);
		btnPathFile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				//
				final String currentEnvironment = buildAttributes.getEnvironment();
				if (cmbWhere.getSelectionIndex() == 1 && (currentEnvironment == null || currentEnvironment.isEmpty())) {
					updateStatus(Messages.PatchSelectServerPage_Environemnt_not_selected);
					return;
				}
				openDiagFile();
				dialogChanged();
			}
		});
		btnPathFile.setText("..."); //$NON-NLS-1$

		final Label lblNewLabel = new Label(container, SWT.NONE);
		lblNewLabel.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 1, 1));
		lblNewLabel.setText(Messages.PatchSelectServerPage_File_without_extension);

		txtFilename = new Text(container, SWT.BORDER);
		txtFilename.setText(""); //$NON-NLS-1$
		txtFilename.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {
				buildAttributes.setFilename(txtFilename.getText());
				overwriteSectionEnable(false);
				overwriteSelection(false);
				dialogChanged();
			}
		});
		txtFilename.setLayoutData(new GridData(SWT.FILL, SWT.TOP, true, false, 1, 1));
		new Label(container, SWT.NONE);
		new Label(container, SWT.NONE);

		lblNewLabel1 = new Label(container, SWT.NONE);
		lblNewLabel1.setText(Messages.PatchSelectServerPage_Leave_blank_default_name);
		new Label(container, SWT.NONE);
		isLocalEnabled();
		new Label(container, SWT.NONE);
		final Composite composite2 = new Composite(container, SWT.NONE);
		composite2.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		composite2.setLayout(new GridLayout(2, false));
		overwrite = new Button(composite2, SWT.CHECK);
		overwrite.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				if (e.widget != null && e.widget instanceof Button) {
					buildAttributes.setOverwrite(((Button) e.widget).getSelection());
					dialogChanged();
				}
			}
		});
		lblOverwrite = new Label(composite2, SWT.NONE);
		lblOverwrite.setText(Messages.PatchSelectServerPage_Override_file);
		overwriteSectionEnable(false);
		new Label(container, SWT.NONE);

		try {
			loadServers();
			selectServer();
			loadAttributes();
			final List<String> envs = loadEnvironments();

			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					try {
						refreshEnvironmentsCombo(envs, buildAttributes.getEnvironmentsList());
					} catch (final Exception e1) {
						ServerUIActivator.logStatus(IStatus.ERROR, e1.getMessage(), e1);
					}
				}
			});
		} catch (final Exception e1) {
			ServerUIActivator.logStatus(IStatus.ERROR, e1.getMessage(), e1);
		}
	}

	private void dialogChanged() {
		boolean hasEnvironmentSelected = treeViewerEnvironments.getCheckedElements().length > 0;
		boolean hasMultiEnvironmentSelected = treeViewerEnvironments.getCheckedElements().length > 1;
		cmbProcesso.setEnabled(hasEnvironmentSelected);
		whereEnabled();
		if (hasEnvironmentSelected || hasMultiEnvironmentSelected) {
			isLocalEnabled();
		}
		String mensagemErro = checkForErrorInPage(hasEnvironmentSelected, hasMultiEnvironmentSelected);
		updateStatus(mensagemErro);
	}

	@Override
	public IWizardPage getNextPage() {
		IWizardPage nextPage = null;
		final BuildPatchWizard patchBuilderWizard = (BuildPatchWizard) getWizard();

		if (buildAttributes.getProcesso() == BuildPatchProcessType.BY_COMPARISON) {
			nextPage = patchBuilderWizard.getComparasionPage();
		} else if (buildAttributes.getProcesso() == BuildPatchProcessType.BY_WORKAREA) {
			final PatchWorkareaPage page = patchBuilderWizard.getWorkareaPage();
			nextPage = page;
		} else if (buildAttributes.getProcesso() == BuildPatchProcessType.BY_RPO) {
			nextPage = patchBuilderWizard.getRpoPage();
		}

		return nextPage;
	}

	private void isLocalEnabled() {
		boolean enable = buildAttributes.isLocal();
		txtPackagePath.setEditable(enable);
		txtPackagePath.setEnabled(enable);
		txtFilename.setEnabled(enable);
		lblNewLabel1.setEnabled(enable);
	}

	private void loadAttributes() {
		final BuildPatchProcessType process = buildAttributes.getProcesso();
		switch (process) {
		case BY_COMPARISON:
			cmbProcesso.select(0);
			break;
		case BY_RPO:
			cmbProcesso.select(1);
			break;
		case BY_WORKAREA:
			cmbProcesso.select(2);
			break;
		default:
			break;
		}
	}

	/**
	 * Efetua a carga dos ambientes do servidor selecionado.
	 *
	 * @return
	 *
	 * @throws Exception
	 */
	private List<String> loadEnvironments() throws Exception {
		final IAppServerInfo serverConnector = buildAttributes.getServer();
		List<String> aux = new ArrayList<String>();

		serverConnector.getEnvironments().forEach(e -> aux.add(e.getName()));

		return aux;
	}

	/**
	 * Efetua a carga de servidores.
	 *
	 * @throws Exception
	 */
	private void loadServers() throws Exception {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		final List<IServerInfo> activeServers = serverManager.getActiveServers(IAppServerInfo.class);
		if (activeServers.isEmpty()) {
			throw new Exception(Messages.PatchSelectServerPage_No_active_servers);
		}

		for (final IServerInfo server : activeServers) {
			final String serverName = server.getName();
			cmbServer.add(serverName);
			serverMap.put(serverName, server);
		}
	}

	protected void openDiagFile() {
		String packagePath = null;

		if (cmbWhere.getSelectionIndex() == 0) {
			packagePath = ServerUIUtil.openFolderDialog(getShell(), Messages.PatchSelectServerPage_Build_patch_title,
					null);
		} else {
			// Remote
//			final IServerDirectoryServerNode serverNode = ServerFileSystemFactory.getInstance()
//					.createServerNode(buildAttributes.getServer(), buildAttributes.getEnvironment(), false);
//			final ServerDirectoryDialog dialog = new ServerDirectoryDialog(this.getShell(), serverNode, false, false);
//			if (dialog.open() == Window.OK) {
//				final IServerDirectoryItemNode item = dialog.getItem();
//				packagePath = (item == null) ? "" : item.getAbsolutPath(); //$NON-NLS-1$
//			}
		}

		if (packagePath != null) {
			txtPackagePath.setText(packagePath);
			buildAttributes.setPatchFilePath(txtPackagePath.getText());
		}
	}

	private void overwriteSectionEnable(final boolean enable) {
		overwrite.setEnabled(enable);
		lblOverwrite.setEnabled(enable);
	}

	private void overwriteSelection(final boolean selection) {
		overwrite.setSelection(selection);
		buildAttributes.setOverwrite(overwrite.getSelection());
	}

	protected void processEnvironmentCheck(final Object element, final boolean isChecked) {
		final String currentEnvironment = buildAttributes.getEnvironment();
		final String environment = (String) element;

		if (isChecked) {
			if (currentEnvironment == null || currentEnvironment.isEmpty()) {
				buildAttributes.setEnvironment(environment);
			}
			if (!buildAttributes.getEnvironmentsList().contains(environment)) {
				buildAttributes.addEnvironment(environment);
			}
		} else {
			buildAttributes.removeEnvironment(environment);
			final String filename = buildAttributes.getFilename();
			if (buildAttributes.getEnvironmentsList().isEmpty()) {
				buildAttributes.setEnvironment(""); //$NON-NLS-1$
			} else if (filename == null || filename.isEmpty()) {
				final String lastEnvironment = buildAttributes.getEnvironmentsList().get(0);
				buildAttributes.setEnvironment(lastEnvironment);
			}
		}
	}

	private void refreshEnvironmentsCombo(final List<String> environmentListFromServer,
			final List<String> checkedEnvironments) {

		updateCheckedEnvironments(environmentListFromServer, checkedEnvironments);

		treeViewerEnvironments.setInput(environmentListFromServer);
		treeViewerEnvironments.setCheckedElements(checkedEnvironments.toArray());

		dialogChanged();
	}

	/**
	 * Pr�-seleciona o servidor corrente.
	 *
	 * @throws Exception
	 */
	private void selectServer() throws Exception {
		String currentServerName;
		try {
			IServerManager serverManager = ServerActivator.getDefault().getServerManager();
			currentServerName = serverManager.getCurrentServer().getName();
			cmbServer.setText(currentServerName);
			final IAppServerInfo server = (IAppServerInfo) serverManager.getServer(cmbServer.getText());
			buildAttributes.setServer(server);
		} catch (final Exception e) {
			currentServerName = null;
			e.printStackTrace();
		}
	}

	private void updateCheckedEnvironments(final List<String> envListFromServer, final List<String> checkedEnvs) {
		cleanupCheckedEnvironments(envListFromServer, checkedEnvs);
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		String currentEnvironment = serverManager.getCurrentServer().getCurrentEnvironment().toUpperCase();
		addEnvironmentToCheckedList(currentEnvironment, checkedEnvs, envListFromServer);
		List<String> multiEnv = buildAttributes.getServer().getMultiEnvironmentSelection();
		addMultiEnvironments(checkedEnvs, multiEnv, envListFromServer);
		if (checkedEnvs.size() == 1) {
			processEnvironmentCheck(checkedEnvs.get(0), true);
		}
	}

	private void updateStatus(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

	private void whereEnabled() {
		final boolean enabled = (buildAttributes.getProcesso() != BuildPatchProcessType.UNDEFINED);
		cmbWhere.setEnabled(enabled);
		txtPackagePath.setEditable(enabled);
		txtPackagePath.setEnabled(enabled);
		btnPathFile.setEnabled(enabled);
	}

}
