package br.com.totvs.tds.ui.server.wizards.patch;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.zip.ZipFile;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;

import com.google.common.io.Files;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.interfaces.IServerReturn;
import br.com.totvs.tds.server.jobs.ApplyPatchAttributes;
import br.com.totvs.tds.server.jobs.ApplyPatchFileReturn;
import br.com.totvs.tds.server.jobs.ApplyPatchFileReturn.MessageType;
import br.com.totvs.tds.server.jobs.ApplyPatchMode;
import br.com.totvs.tds.server.jobs.ProcessPatchJob;
import br.com.totvs.tds.server.model.SourceInformation;
import br.com.totvs.tds.ui.TDSUtil;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.ServerUIIcons;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryItemNode;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryServerNode;
import br.com.totvs.tds.ui.server.providers.ApplyPatchContentProvider;
import br.com.totvs.tds.ui.server.providers.ColumnAplicationLabelProvider;
import br.com.totvs.tds.ui.server.providers.ColumnIconsLabelProvider;
import br.com.totvs.tds.ui.server.providers.ColumnProgramLabelProvider;
import br.com.totvs.tds.ui.server.widget.ServerDirectoryDialog;

/**
 * The apply patch page definition.
 *
 * @author daniel.yampolschi
 *
 */

public class ApplyPatchPage extends WizardPage {

	private final ApplyPatchAttributes attributes;

	private Combo cmbServer;
	private Combo cmbEnvironment;
	private Combo cmbLocation;

	private TableViewer tableViewer;

	private Button btnRemovePathFile;
	private Button btnRemoveAppliedPatchFile;
	private Button btnDetailPathFile;
	private Button btnIntegrity;

	private SelectionListener serverSelectionListener;
	private SelectionListener environmentSelectionListener;
	private SelectionListener locationSelectionListener;
	private SelectionListener selectPatchFileSelectionAdapter;
	private ISelectionChangedListener tableSelectionListener;

	private ApplyPatchFileReturn currentSelection;

	/**
	 * This is a list that must hold all local patches added to the table to be
	 * applied.<br>
	 * The value to be added to the list must be the patch full path.
	 */
	private List<IPath> localPatchesAdded = new ArrayList<IPath>();
	private Map<String, IAppServerInfo> serverMap = new HashMap<String, IAppServerInfo>();
	private List<String> validPatchExtensions = Arrays.asList(new String[] { "ptm", "upd", "pak" }); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$

	private final String[] patchFilterNames = new String[] { "Pacote atualização (*.ptm)", "Pacote atualização (*.upd)",
			"Pacote atualização (*.pak)", "Pacote compactado (*.zip)", "Todos os arquivos (*.*)" }; //$NON-NLS-3$ ;
	private final String[] patchFilterExtensions = new String[] { "*.ptm", //$NON-NLS-1$
			"*.upd", //$NON-NLS-1$
			"*.pak", //$NON-NLS-1$
			"*.zip", //$NON-NLS-1$
			"*.*" }; //$NON-NLS-1$

	private ColumnProgramLabelProvider programLabel;

	private ApplyPatchMode modeSave;

	private IServerManager serverManager;

	private Label lblInfoPackage;

	private Button btnAutoValidate;
	// private boolean appliedPatches;

	/**
	 * Create the wizard.
	 *
	 * @param attributes atributos
	 */
	public ApplyPatchPage(final ApplyPatchAttributes attributes) {
		super("applyPatchPage"); //$NON-NLS-1$
		setImageDescriptor(ServerUIIcons.getPatchCorrection());
		setTitle("Aplicação de Pacote de Atualização");
		setDescription("Este assistente o auxiliará na aplicação de Pacotes de Atualização.");
		this.attributes = attributes;

		IWorkbench serviceLocator = PlatformUI.getWorkbench();
		serviceLocator.getService(ILanguageServerService.class);
		serverManager = serviceLocator.getService(IServerManager.class);
	}

	private boolean addFilesToTheTree(final String[] fileNames, boolean local) throws IOException {

		for (String fileName : fileNames) {
			if (fileName != null && !fileName.isEmpty()) {
				String fileExtension = FilenameUtils.getExtension(fileName);
				if (fileExtension.equalsIgnoreCase("zip")) { //$NON-NLS-1$
					addPatchesFromZip(fileName);
				} else {
					ApplyPatchFileReturn applyPatchFileReturn = createApplyPatchFileReturn(fileName, local);
					attributes.addApplyPatchFileReturn(applyPatchFileReturn);
				}
			}
		}

		return true;
	}

	private void addPatchesFromZip(final String fullPathFile) throws IOException {
		ZipFile zippedPatch = new ZipFile(fullPathFile);
		File tempDir = Files.createTempDir();
		String destinationFolder = tempDir.getAbsolutePath();
		List<File> unzipFiles = TDSUtil.unzipFile(zippedPatch, destinationFolder, true);
		List<File> selectFiles = openSelectorPatches(zippedPatch.getName(), unzipFiles);

		boolean patchFound = false;
		for (File file : selectFiles) {
			String fileExtension = FilenameUtils.getExtension(file.getName());
			if (validPatchExtensions.contains(fileExtension.toLowerCase())) {
				ApplyPatchFileReturn applyPatchFileReturn = createApplyPatchFileReturn(file.getAbsolutePath(), true);
				applyPatchFileReturn.setTemporary(true);
				applyPatchFileReturn.setOriginalFile(fullPathFile.toString().concat("#").concat(file.getName()));
				applyPatchFileReturn.setMessageType(MessageType.NEW_ZIP);
				attributes.addApplyPatchFileReturn(applyPatchFileReturn);

				patchFound = true;
			}
		}

		if (!patchFound) {
			throw new IOException("Nenhum Pacote de Atualização encontrado no arquivo selecionado.");
		}

		FileUtils.forceDeleteOnExit(new File(destinationFolder));
	}

	private void buildPacotesComposite(final Composite composite) {
		Label lblPacote = new Label(composite, SWT.NONE);
		lblPacote.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 3, 1));
		lblPacote.setText("Pacotes");

		tableViewer = new TableViewer(composite, SWT.BORDER | SWT.FULL_SELECTION);
		Table table = tableViewer.getTable();
		GridData gd_table = new GridData(GridData.FILL_BOTH);
		gd_table.horizontalSpan = 2;
		table.setLayoutData(gd_table);
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		TableViewerColumn tvcPrograma = new TableViewerColumn(tableViewer, SWT.NONE);
		tvcPrograma.getColumn().setWidth(150);
		tvcPrograma.getColumn().setText("Pacotes");
		programLabel = new ColumnProgramLabelProvider();
		tvcPrograma.setLabelProvider(programLabel);

		TableViewerColumn tvcValido = new TableViewerColumn(tableViewer, SWT.NONE);
		tvcValido.getColumn().setWidth(150);
		tvcValido.getColumn().setText("Validação");
		tvcValido.setLabelProvider(new ColumnAplicationLabelProvider());

		TableViewerColumn tvcErros = new TableViewerColumn(tableViewer, SWT.NONE);
		tvcErros.getColumn().setWidth(200);
		tvcErros.getColumn().setText("Erros/Avisos");
		tvcErros.setLabelProvider(new ColumnLabelProvider() {
			@Override
			public String getText(final Object element) {
				ApplyPatchFileReturn applyPatchFileReturn = (ApplyPatchFileReturn) element;
				MessageType messageType = applyPatchFileReturn.getMessageType();
				String msg = applyPatchFileReturn.getValidationMessage();
				if (messageType != null && messageType.equals(MessageType.OK) && (msg == null || msg.isEmpty())) {
					return "OK";
				}
				return msg == null ? "" : msg;
			}
		});

		TableViewerColumn tvcIcons = new TableViewerColumn(tableViewer, SWT.NONE);
		tvcIcons.getColumn().setWidth(35);
		tvcIcons.getColumn().setText(""); //$NON-NLS-1$
		tvcIcons.setLabelProvider(new ColumnIconsLabelProvider());

		tableViewer.setContentProvider(new ApplyPatchContentProvider());
		tableViewer.setInput(attributes.getApplyPatchFilesReturn());
		tableViewer.addSelectionChangedListener(tableSelectionListener);

		Composite compBrowse = new Composite(composite, SWT.NONE);
		GridLayout gl_compBrowse = new GridLayout(1, false);
		gl_compBrowse.horizontalSpacing = 0;
		gl_compBrowse.marginWidth = 0;
		compBrowse.setLayout(gl_compBrowse);
		compBrowse.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, true, 1, 1));

		Button btnAddPathFile = new Button(compBrowse, SWT.NONE);
		btnAddPathFile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnAddPathFile.addSelectionListener(selectPatchFileSelectionAdapter);
		btnAddPathFile.setText("Adicionar");

		btnAutoValidate = new Button(compBrowse, SWT.CHECK);
		btnAutoValidate.setToolTipText("Valida em segundo plano o pacote recem adicionado.");
		btnAutoValidate.setSelection(true);
		btnAutoValidate.setLayoutData(new GridData(SWT.RIGHT, SWT.FILL, true, true, 1, 1));
		btnAutoValidate.setText("Validar");

		btnRemovePathFile = new Button(compBrowse, SWT.NONE);
		btnRemovePathFile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnRemovePathFile.setText("Remover");
		btnRemovePathFile.setEnabled(false);
		btnRemovePathFile.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				widgetSelected(e);
			}

			@Override
			public void widgetSelected(final SelectionEvent e) {
				clearApplyPatchFilesReturn(currentSelection);
			}
		});

		btnDetailPathFile = new Button(compBrowse, SWT.NONE);
		btnDetailPathFile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnDetailPathFile.setText("Detalhar");
		btnDetailPathFile.setEnabled(false);
		btnDetailPathFile.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				widgetSelected(e);
			}

			@Override
			public void widgetSelected(final SelectionEvent e) {
				showPatchDetails(currentSelection);
			}
		});

		btnIntegrity = new Button(compBrowse, SWT.NONE);
		btnIntegrity.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnIntegrity.setText("Integridade");
		btnIntegrity.setEnabled(false);
		btnIntegrity.setToolTipText("Verifica se o pacote de atualização está válido");

		btnIntegrity.addSelectionListener(new SelectionListener() {

			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				widgetSelected(e);
			}

			@Override
			public void widgetSelected(final SelectionEvent e) {
				try {
					showIntegrity(currentSelection);
				} catch (Exception e1) {
					e1.printStackTrace();
				}
			}
		});

		btnRemoveAppliedPatchFile = new Button(compBrowse, SWT.NONE);
		btnRemoveAppliedPatchFile.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		btnRemoveAppliedPatchFile.setText("Remover aplicados");
		btnRemoveAppliedPatchFile.setEnabled(false);
		btnRemoveAppliedPatchFile.addSelectionListener(new SelectionListener() {
			@Override
			public void widgetDefaultSelected(final SelectionEvent e) {
				widgetSelected(e);
			}

			@Override
			public void widgetSelected(final SelectionEvent e) {
				removeAppliedPatches();
			}
		});
	}

	public void clearApplyPatchFilesReturn(final ApplyPatchFileReturn applyPatchFileReturn) {
		// tableViewer.setInput(new ArrayList<>());
		attributes.getApplyPatchFilesReturn().remove(applyPatchFileReturn);
		localPatchesAdded.remove(applyPatchFileReturn.getPatchFile());
		deleteTemporaryPatchFile(applyPatchFileReturn);
		// setInput colocado nesse ponto para forçar a chamada ao método
		// inputChanged
		// do content provider e fazer corretamente o dispose de qualquer widget
		// (como por exemplo imagem de link) que exista na tabela.
		tableViewer.setInput(attributes.getApplyPatchFilesReturn());
		refreshTable(true);
	}

	public void clearTemporaryPatches() {
		if (attributes != null) {
			List<ApplyPatchFileReturn> applyPatchFilesReturn = attributes.getApplyPatchFilesReturn();
			for (ApplyPatchFileReturn applyPatchFileReturn : applyPatchFilesReturn) {
				if (applyPatchFileReturn.getMessageType().equals(MessageType.OK)) {
					deleteTemporaryPatchFile(applyPatchFileReturn);
				}
			}
		}
	}

	private ApplyPatchFileReturn createApplyPatchFileReturn(final String fullPath, boolean local) {
		Path path = new Path(fullPath);
		ApplyPatchFileReturn applyPatchFileReturn = new ApplyPatchFileReturn(path);
		localPatchesAdded.add(path);
		applyPatchFileReturn.setLocal(local);
		return applyPatchFileReturn;
	}

	@Override
	public void createControl(final Composite parent) {
		Composite container = new Composite(parent, SWT.NULL);

		setControl(container);

		createSelectionListeners();
		container.setLayout(new GridLayout(2, false));
		Label lblServidor = new Label(container, SWT.NONE);
		lblServidor.setText("Servidor");

		cmbServer = new Combo(container, SWT.READ_ONLY);
		cmbServer.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cmbServer.addSelectionListener(serverSelectionListener);

		Label lblAmbiente = new Label(container, SWT.NONE);
		lblAmbiente.setText("Ambiente");

		cmbEnvironment = new Combo(container, SWT.READ_ONLY);
		cmbEnvironment.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		cmbEnvironment.addSelectionListener(environmentSelectionListener);

		Label lblNewLabel = new Label(container, SWT.NONE);
		lblNewLabel.setText("Origem do pacote");

		cmbLocation = new Combo(container, SWT.READ_ONLY);
		cmbLocation.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		cmbLocation.setItems(new String[] { "Local", "Remoto" });
		cmbLocation.select(0);
		cmbLocation.addSelectionListener(locationSelectionListener);
		//
		Composite composite_1 = new Composite(container, SWT.NONE);
		composite_1.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, false, 2, 1));
		composite_1.setLayout(new GridLayout(3, false));
		buildPacotesComposite(composite_1);

		lblInfoPackage = new Label(container, SWT.NONE);
		lblInfoPackage.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 2, 1));
		lblInfoPackage.setText("Detalhes do pacote selecionado.");

		try {
			initialize();
		} catch (Exception e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}
	}

	private void createSelectionListeners() {
		serverSelectionListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent evt) {
				cmbEnvironment.removeAll();
				attributes.setEnvironment(null);
				try {
					IAppServerInfo serverInfo = serverMap.get(cmbServer.getText());
					attributes.setCurrentAppServer(serverInfo);
					if (serverInfo != null && serverInfo.isConnected()) {
						loadEnvironments();
					}
				} catch (Exception e) {
					ServerUIActivator.showStatus(IStatus.ERROR, e.getMessage(), e);
				}

				dialogChanged();
			}
		};
		environmentSelectionListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				attributes.setEnvironment(cmbEnvironment.getText());
				dialogChanged();
			}
		};

		locationSelectionListener = new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				dialogChanged();
			}
		};

		selectPatchFileSelectionAdapter = new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				try {
					selectApplyPatchFileDialog();
					dialogChanged();

					if (btnAutoValidate.getSelection()) {
						ProcessPatchJob pp = new ProcessPatchJob("autoValidade", attributes) {

							@Override
							protected IStatus run(IProgressMonitor monitor) {
								IStatus status = null;

								try {
									status = doValidatePatchs(monitor);
								} catch (Exception e) {
									status = new Status(IStatus.ERROR, ServerUIActivator.PLUGIN_ID, e.getMessage(), e);
									e.printStackTrace();
								}
								return status;
							}
						};
						pp.join();

						showValidationErrors();
					}
				} catch (IllegalArgumentException exception) {
					ServerUIActivator.showStatus(IStatus.ERROR, exception.getMessage(), exception);
				} catch (IOException e1) {
					ServerUIActivator.showStatus(IStatus.ERROR, e1.getMessage(), e1);
				} catch (Exception e1) {
					// TODO Auto-generated catch block
					e1.printStackTrace();
				}
			}
		};

		tableSelectionListener = new ISelectionChangedListener() {
			@Override
			public void selectionChanged(final SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();
				Object object = selection.getFirstElement();
				ApplyPatchFileReturn currentSelection = null;
				if (object instanceof ApplyPatchFileReturn) {
					currentSelection = (ApplyPatchFileReturn) object;
				}
				setCurrentPatchSelection(currentSelection);
				updateButtons();
			}
		};

	}

	private void deleteTemporaryPatchFile(final ApplyPatchFileReturn applyPatchFileReturn) {
		if (applyPatchFileReturn.isTemporary()) {
			String patchFullPath = applyPatchFileReturn.getPatchFullPath();
			File tmpFile = new File(patchFullPath);
			if (tmpFile.exists()) {
				tmpFile.delete();
			}
		}
	}

	private void dialogChanged() {
		try {
			String serverMessage = verifyServerConditions();
			if (serverMessage != null) {
				updateStatus(serverMessage);
				return;
			}
			List<ApplyPatchFileReturn> applyPatchFilesReturn = attributes.getApplyPatchFilesReturn();
			if (applyPatchFilesReturn == null || applyPatchFilesReturn.isEmpty()) {
				updateStatus("Selecionar ao menos um pacote de atualiza\\u00E7\\u00E3o.");
				return;
			} else {
				btnRemoveAppliedPatchFile.setEnabled(false);
				// verifica o estado dos Pacotes de atualizações na tabela
				for (ApplyPatchFileReturn applyPatchFileReturn : applyPatchFilesReturn) {
					MessageType messageType = applyPatchFileReturn.getMessageType();
					if (messageType.equals(MessageType.OK)) {
						updateStatus("Existem pacotes j\\u00E1 aplicados. Remova-os para prosseguir.");
						btnRemoveAppliedPatchFile.setEnabled(true);
						return;
					}
				}
			}
			updateStatus(null);
			tableViewer.refresh();
		} catch (Exception e) {
			ServerUIActivator.showStatus(IStatus.ERROR, e.getMessage(), e);
		}
	}

	// private void addLinkToItems() {
	// Table table = tableViewer.getTable();
	// TableItem[] items = table.getItems();
	// for (TableItem item : items) {
	// addImageHyperLink(item, table);
	// }
	// }
	//
	// private void addImageHyperLink(final TableItem item, final Table table) {
	// ApplyPatchFileReturn applyPatchFileReturn = (ApplyPatchFileReturn)
	// item.getData();
	// if (applyPatchFileReturn != null) {
	// String documentationURL = applyPatchFileReturn.getDocumentationURL();
	// if (documentationURL != null && !documentationURL.isEmpty()) {
	// int iconColumnIndex = 3;
	// ImageHyperlink link = createImageHyperLinkItem(documentationURL, table);
	// TableEditor tableEditor = new TableEditor(table);
	// tableEditor.grabHorizontal = true;
	// tableEditor.setEditor(link, item, iconColumnIndex);
	// }
	// }
	// }
	//
	// private ImageHyperlink createImageHyperLinkItem(final String
	// documentationURL, final Table table) {
	// ImageHyperlink link = new ImageHyperlink(table, SWT.FILL |
	// SWT.READ_ONLY);
	// link.setHref(documentationURL);
	// Cursor cursor = new Cursor(link.getDisplay(), SWT.CURSOR_HAND);
	// link.setCursor(cursor);
	// link.setImage(SdkUIIcons.getLinkIcon().createImage());
	// link.addHyperlinkListener(new HyperlinkAdapter() {
	// @Override
	// public void linkActivated(final
	// org.eclipse.ui.forms.events.HyperlinkEvent e) {
	// String href = String.valueOf(e.getHref());
	// try {
	// SdkUIUtil.openExternalBrowser(href);
	// } catch (PartInitException e1) {
	// String message = String.format("N�o foi poss�vel acionar a URL: %s",
	// href);
	// ServerUIActivator.showStatus(IStatus.ERROR, message);
	// } catch (MalformedURLException e1) {
	// String message = String.format("A URL informada � inv�lida: %s", href);
	// ServerUIActivator.showStatus(IStatus.ERROR, message);
	// }
	// }
	//
	// });
	// link.setBackground(new Color(link.getDisplay(), new RGB(255, 255, 255)));
	// return link;
	// }

	public IServerInfo getSelectedAppServer() {
		int selectionIndex = cmbServer.getSelectionIndex();
		if (selectionIndex == -1) {
			return null;
		}
		String serverName = cmbServer.getItem(selectionIndex);
		IServerInfo iServerInfo = serverMap.get(serverName);
		return iServerInfo;
	}

	private void initialize() throws Exception {
		loadServers();
		selectServer();
		loadEnvironments();
	}

	private void loadEnvironments() throws Exception {
		final IAppServerInfo serverConnector = attributes.getCurrentAppServer();
		String currentEnvironment = attributes.getEnvironment();
		List<String> environmentList = serverConnector.getEnvironments().stream().map(IEnvironmentInfo::getName)
				.collect(Collectors.toList());
		int select = 0;

		cmbEnvironment.add(""); //$NON-NLS-1$
		for (int idx = 0; idx < environmentList.size(); idx++) {
			String environment = environmentList.get(idx);
			cmbEnvironment.add(environment);
			if (currentEnvironment != null) {
				if (currentEnvironment.equalsIgnoreCase(environment)) {
					select = idx + 1;
				}
			} else {
				if (environment.equalsIgnoreCase(currentEnvironment)) {
					select = idx + 1;
					attributes.setEnvironment(currentEnvironment);
				}
			}
		}
		cmbEnvironment.select(select);
	}

	private void loadServers() {
		List<IServerInfo> activeServers = attributes.getServerList();

		if (activeServers.isEmpty()) {
			ServerUIActivator.showStatus(IStatus.ERROR,
					"Nenhum servidor ativo encontrado para utilização desta funcionalidade.");
		}

		for (IServerInfo server : activeServers) {
			String serverName = server.getName();
			cmbServer.add(serverName);
			serverMap.put(serverName, (IAppServerInfo) server);
		}
	}

	private void logOptionSelected(final ApplyPatchFileReturn applyPatchFileReturn, final ApplyPatchMode applyMode) {
		String logMessage = ""; //$NON-NLS-1$
		if (applyMode.equals(ApplyPatchMode.APPLY_ALL)) {
			logMessage = "Aplicar todos";
		} else if (applyMode.equals(ApplyPatchMode.APPLY_NEWEST_ONLY)) {
			logMessage = "Somente atualizados";
		}

		ServerUIActivator.logStatus(IStatus.WARNING,
				"[%s] Selecionada a op\\u00E7\\u00E3o %s para aplica\\u00E7\\u00E3o deste pacote.\n\t",
				applyPatchFileReturn.getPatchFile(), logMessage);
	}

	public void monitorUpdated(final IProgressMonitor monitor, final Object object, final String message,
			final int worked) {
		if (monitor != null && !monitor.isCanceled()) {
			if (object != null && object instanceof ApplyPatchFileReturn) {
				final ApplyPatchFileReturn patchFileReturn = (ApplyPatchFileReturn) object;
				Display.getDefault().syncExec(new Runnable() {
					@Override
					public void run() {
						@SuppressWarnings("unchecked")
						List<ApplyPatchFileReturn> input = (List<ApplyPatchFileReturn>) tableViewer.getInput();
						int indexOf = input.indexOf(patchFileReturn);
						patchFileReturn.setValidationMessage(message);
						input.set(indexOf, patchFileReturn);
						tableViewer.getTable().setSelection(indexOf);
						tableViewer.refresh();
					}
				});
			}
		}
	}

	/**
	 * Open a dialog to the user select the patch and <br>
	 * adds the file selected to the attributes.
	 *
	 * @param fFileNames
	 * @return <b>true</b> - if the file was added to the attributes.
	 * @throws IOException
	 */
	private boolean openLocalFileDialog() throws IOException {
		String fileFullPath = TDSUtil.fileDialog(getShell(), patchFilterExtensions, patchFilterNames, true);

		return addFilesToTheTree(fileFullPath.split(";"), true);
	}

	private void openRemoteFileDialog() {
		// Remote
//		IServerDirectoryServerNode serverNode = NodeFactory.getInstance()
//				.createServerNode(attributes.getCurrentAppServer(), attributes.getEnvironment(), true);

		IServerDirectoryServerNode serverNode = null;
		ServerDirectoryDialog dialog = new ServerDirectoryDialog(this.getShell(), serverNode, true, true);
		String patchPath = null;
		if (dialog.open() == Window.OK) {
			List<IServerDirectoryItemNode> items = dialog.getItems();

			for (IServerDirectoryItemNode item : items) {
				patchPath = (item == null) ? "" : item.getAbsolutPath(); //$NON-NLS-1$

				if (patchPath != null && !patchPath.isEmpty()) {
					ApplyPatchFileReturn applyPatchFileReturn = new ApplyPatchFileReturn(patchPath);
					applyPatchFileReturn.setLocal(false);
					attributes.addApplyPatchFileReturn(applyPatchFileReturn);
				}
			}
		}

	}

	private List<File> openSelectorPatches(String zipFilename, List<File> sourceList) {
		List<File> resultList = new ArrayList<File>();

//		List<DualListItem> sourceFiles = new ArrayList<DualListItem>();
//		List<DualListItem> selectedFiles = new ArrayList<DualListItem>();
//
//		IAppServerInfo server = attributes.getCurrentAppServer();
//		String environment = attributes.getEnvironment();
//		String prefixTarget = ""; //$NON-NLS-1$
//		try {
//			IServerIniEnvironment environmentInfo = server.getServerIni().getSectionEnvironment(environment);
//			prefixTarget = environmentInfo.getRpoName().toUpperCase();
//		} catch (Exception e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//			prefixTarget = "@"; //$NON-NLS-1$
//		}
//
//		for (File file : sourceList) {
//			String fileExtension = FilenameUtils.getExtension(file.getName()).toLowerCase();
//			if (validPatchExtensions.contains(fileExtension)) {
//				DualListItem dli = new DualListItem(file.getName(), file);
//				dli.setSelected(file.getName().toUpperCase().startsWith(prefixTarget));
//
//				sourceFiles.add(dli);
//			}
//		}
//
//		if (sourceFiles.isEmpty()) {
//			setErrorMessage(Messages.ApplyPatchPage_36);
//		} else if (sourceFiles.size() == 1) {
//			selectedFiles.add(sourceFiles.get(0));
//		} else {
//			String msg = String.format(Messages.ApplyPatchPage_38, zipFilename);
//			DualSelectorDialog dialog = new DualSelectorDialog(shell, Messages.ApplyPatchPage_39, msg, this.getImage());
//			dialog.setTitleSource(Messages.ApplyPatchPage_40);
//			dialog.setTitleTarget(Messages.ApplyPatchPage_40);
//			dialog.setItems(sourceFiles);
//
//			if (dialog.open() == Window.OK) {
//				for (DualListItem item : dialog.getItems()) {
//					if (item.isSelected()) {
//						resultList.add((File) item.getData());
//					}
//				}
//
//			}
//		}

		return resultList;
	}

	public void refreshTable(final boolean pageChanged) {
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				if (!tableViewer.getControl().isDisposed()) {
					tableViewer.refresh();
					if (pageChanged) {
						dialogChanged();
					}
				}
			}
		});
	}

	protected void removeAppliedPatches() {
		// verifica o estado dos Pacotes de atualizações na tabela
		List<ApplyPatchFileReturn> applyPatchFilesReturn = attributes.getApplyPatchFilesReturn();
		if (applyPatchFilesReturn == null || applyPatchFilesReturn.isEmpty()) {
			return;
		}
		List<ApplyPatchFileReturn> appliedPatchFiles = new ArrayList<ApplyPatchFileReturn>();
		for (ApplyPatchFileReturn applyPatchFileReturn : applyPatchFilesReturn) {
			MessageType messageType = applyPatchFileReturn.getMessageType();
			if (messageType.equals(MessageType.OK)) {
				appliedPatchFiles.add(applyPatchFileReturn);
			}
		}
		for (ApplyPatchFileReturn applyPatchFileReturn : appliedPatchFiles) {
			attributes.removerApplyPatchFileReturn(applyPatchFileReturn);
			deleteTemporaryPatchFile(applyPatchFileReturn);
		}
		dialogChanged();
	}

	private boolean selectApplyPatchFileDialog() throws IOException {
		boolean canUpdateTable = false;

		if (cmbLocation.getSelectionIndex() == 0) {
			// LOCAL
			canUpdateTable = openLocalFileDialog();
		} else {
			// REMOTE
			openRemoteFileDialog();
		}
//		if (canUpdateTable) {
//			refreshTable(false);
//		}
		return canUpdateTable;
	}

	private void selectServer() throws Exception {
		String currentServerName;
		try {
			currentServerName = serverManager.getCurrentServer().getName();
			String[] items = cmbServer.getItems();
			for (int index = 0; index < items.length; index++) {
				String itemName = items[index];
				if (itemName.equals(currentServerName)) {
					cmbServer.select(index);
					break;
				}
			}
		} catch (Exception e) {
			currentServerName = null;
			e.printStackTrace();
		}
	}

	protected void setCurrentPatchSelection(final ApplyPatchFileReturn currentSelection) {
		this.currentSelection = currentSelection;

		if (this.currentSelection != null) {
			lblInfoPackage.setText(String.format("Pacote: %s (~%.2f KB) Situação: %s",
					currentSelection.getPatchFullPath(), currentSelection.getSize() / 1024.0,
					currentSelection.isValidated() ? "OK" : "Com erro ou restrição"));
		}
	}

	protected void showIntegrity(final ApplyPatchFileReturn currentSelection) throws Exception {
		IAppServerInfo server = attributes.getCurrentAppServer();
		String env = attributes.getEnvironment();
		boolean local = currentSelection.isLocal();

		ArrayList<URI> patchList = new ArrayList<URI>();
		patchList.add(currentSelection.getPatchFile().toFile().toURI());

		IServerReturn serverReturn = server._getPatchIntegrity(env, patchList, local);
		if (serverReturn.isOperationOk()) {
			MessageDialog.openInformation(getShell(), "Integridade do Pacote de Atualização",
					serverReturn.getReturnMessage());
		} else {
			updateStatus("O pacote de atualização não está integro.");
			ServerUIActivator.logStatus(IStatus.ERROR, serverReturn.getReturnMessage());
		}
	}

	private void showPatchDetails(final ApplyPatchFileReturn applyPatchFileReturn) {
		showPatchDetails(applyPatchFileReturn.getPatchFullPath());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * br.com.totvs.tds.sdk.ui.patch.IApplyPatchPage#showPatchDetails(java.lang.
	 * String)
	 */
	public void showPatchDetails(final String patchFile) {
		try {
			IAppServerInfo server = attributes.getCurrentAppServer();
			String environment = attributes.getEnvironment();
			List<SourceInformation> detalhesDoPatch = PatchApplySupport.getPatchDetails(patchFile, server, environment,
					cmbLocation.getSelectionIndex());
			PatchLogDetailsDialog dialog = new PatchLogDetailsDialog(getShell(), detalhesDoPatch);
			dialog.open();
		} catch (Exception e) {
			ServerUIActivator.showStatus(IStatus.ERROR, e.getMessage(), e);
		}
	}

	private void showValidationError(final ApplyPatchFileReturn applyPatchFileReturn) {
		List<String[]> oldPrograms = applyPatchFileReturn.getOldPrograms();
		PatchValidationDetailsDialog dialog = new PatchValidationDetailsDialog(getShell(), oldPrograms,
				applyPatchFileReturn);

		ApplyPatchMode applyMode = ApplyPatchMode.VALIDATE_PATCH;
		if (dialog.open() == Window.OK) {
			applyMode = dialog.getApplyMode();
			if (dialog.isSaveAction()) {
				modeSave = applyMode;
			}
			logOptionSelected(applyPatchFileReturn, applyMode);
		} else {
			ServerUIActivator.logStatus(IStatus.CANCEL,
					"Seleção de opção para aplicação de pacote cancelada.\n\tPacote: %s",
					applyPatchFileReturn.getPatchFile());
		}
		applyPatchFileReturn.setApplyMode(applyMode);

		dialogChanged();
	}

	/**
	 * Percorre a lista de Pacotes e verifica se existe algum com erro de validação.
	 * Se houver apresenta o dialogo de erros de validacao deste pacote.
	 */
	public void showValidationErrors() {
		List<ApplyPatchFileReturn> applyPatchFilesReturnList = attributes.getApplyPatchFilesReturn();

		for (ApplyPatchFileReturn applyPatchFileReturn : applyPatchFilesReturnList) {
			if (applyPatchFileReturn.getMessageType() == MessageType.ERROR) {
				updateStatus("Um dos pacotes de atualizações contem erros.");
				break;
			}
			if (applyPatchFileReturn.getApplyMode() == ApplyPatchMode.VALIDATE_ERROR && modeSave == null) {
				showValidationError(applyPatchFileReturn);
				if (modeSave == null) {
					break;
				}
			}

			if (modeSave != null) {
				applyPatchFileReturn.setApplyMode(modeSave);
				dialogChanged();
			}
		}
	}

	private void updateButtons() {
		btnRemovePathFile.setEnabled(currentSelection != null);
		btnDetailPathFile.setEnabled(currentSelection != null);
		btnIntegrity.setEnabled(currentSelection != null);
	}

	private void updateStatus(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

	private String verifyServerConditions() throws Exception {
		String message = null;
		if (cmbServer.getItems().length == 0) {
			message = "Não existe servidor conectado para aplicação do pacote de atualização.";
		}
		if (attributes.getCurrentAppServer() == null) {
			message = "Selecionar o servidor para instalação do pacote de atualização.";
		} else {
			IAppServerInfo currentAppServer = attributes.getCurrentAppServer();

			if (!currentAppServer.isConnected()) {
				message = "O servidor selecionado não esta conectado.";
			} else if (!currentAppServer.canPermission("APPLY_PATCH")) {
				message = "O servidor selecionado n\u00E3o permite a aplica\u00E7\u00E3o de patchs por esta m\u00E1quina TDS.\n"
						+ "Caso necessite, pe\u00E7a ao administrador que acrescente esta m\u00E1quina"
						+ "\u00E0 lista com permiss\u00F5es de aplicar patch.";

			} else if (attributes.getEnvironment() == null || attributes.getEnvironment().isEmpty()) {
				message = "Selecionar o ambiente para instala\\u00E7\\u00E3o do pacote.";
			}
		}

		return message;
	}

}