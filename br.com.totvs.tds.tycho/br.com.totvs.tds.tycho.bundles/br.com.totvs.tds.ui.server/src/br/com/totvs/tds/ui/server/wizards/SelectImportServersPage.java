package br.com.totvs.tds.ui.server.wizards;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ILabelDecorator;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IDecoratorManager;
import org.eclipse.ui.PlatformUI;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.tools.ExportTool;
import br.com.totvs.tds.server.tools.ImportTools;
import br.com.totvs.tds.server.xml.Group;
import br.com.totvs.tds.server.xml.ObjectFactory;
import br.com.totvs.tds.server.xml.XMLServerRoot;
import br.com.totvs.tds.server.xml.XMLVersionControl;
import br.com.totvs.tds.ui.server.internal.provider.ImportServerDecoratingLabelProvider;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.views.ServerViewContentProvider;
import br.com.totvs.tds.ui.server.vo.ServerImporExportAttributesVO;

/**
 * Seleção de servidores para importação.
 *
 * @author acandido
 */
public class SelectImportServersPage extends WizardPage {

	private ServerImporExportAttributesVO attributes;
	private Button btnSelectFile;
	private Button cbOverwrite;
	private Combo cbSelectTarget;
	private CheckboxTreeViewer checkboxTreeViewer;
	private Tree checkTreeViewerProjetos;
	private Label lblDestino;
	private Label lblSource;
	private ArrayList<IItemInfo> nodeList;
	private final ObjectFactory serverTreeFactory = new ObjectFactory();
	private Shell shell;
	private Text txtCaminhoDoPatch;
	protected List<IItemInfo> duplicatedItemsList = new ArrayList<IItemInfo>();

	/**
	 * Create the wizard.
	 *
	 * @param expAttributes
	 */
	public SelectImportServersPage(final ServerImporExportAttributesVO impAttributes) {
		super("SelectImportServersView"); //$NON-NLS-1$
		setTitle(Messages.SelectImportServersPage_selection_page_title);
		setDescription(Messages.SelectImportServersPage_selection_one_or_more_server);
		attributes = impAttributes;
	}

	@Override
	public boolean canFlipToNextPage() {
		if (attributes.getRepeatedServerNames().size() > 0) {
			return super.canFlipToNextPage();
		}

		return false;
	}

	private String changeExtension(final String originalName, String newExtension) {
		int lastDot = originalName.lastIndexOf("."); //$NON-NLS-1$
		if (newExtension.startsWith("*.")) //$NON-NLS-1$
			newExtension = newExtension.substring(1);
		if (lastDot != -1) {
			return originalName.substring(0, lastDot) + newExtension;
		}

		return originalName + newExtension;
	}

	@SuppressWarnings("unlikely-arg-type")
	private void checkServersSelecionados(final TreeItem[] items) {
		Map<String, IItemInfo> mapFontes = attributes.getItemsSelected();
		for (int i = 0; i < items.length; i++) {
			TreeItem item = items[i];
			if (mapFontes.containsValue(item.getText())) {
				item.setChecked(true);
				System.out.println("SelectImportServersPage.checkServersSelecionados()"); //$NON-NLS-1$
				// UiUtils.checkPath(item.getParentItem(), true, true);
			}
			checkServersSelecionados(item.getItems());
		}
	}

	/**
	 * Create contents of the wizard.
	 *
	 * @param parent
	 */
	@Override
	public void createControl(final Composite parent) {
		Composite container = new Composite(parent, SWT.NONE);
		shell = parent.getShell();
		setTitle(getWizard().getWindowTitle());
		setControl(container);
		container.setLayout(new GridLayout(3, false));
		lblSource = new Label(container, SWT.NONE);
		lblSource.setText(Messages.SelectImportServersPage_file);
		txtCaminhoDoPatch = new Text(container, SWT.BORDER | SWT.READ_ONLY);
		txtCaminhoDoPatch.setEnabled(false);
		txtCaminhoDoPatch.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		btnSelectFile = new Button(container, SWT.NONE);
		btnSelectFile.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnSelectFile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				openDialogFile();
			}
		});
		btnSelectFile.setText("..."); //$NON-NLS-1$
		lblDestino = new Label(container, SWT.NONE);
		lblDestino.setText(Messages.SelectImportServersPage_target);
		cbSelectTarget = new Combo(container, SWT.READ_ONLY);
		cbSelectTarget.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
		cbSelectTarget.addSelectionListener(new SelectionAdapter() {
			// ////@Override
			@Override
			public void widgetSelected(final SelectionEvent e) {
				dialogChanged();
			}
		});
		new Label(container, SWT.NONE);
		fillComboOptions();
		Label lblProjetos = new Label(container, SWT.NONE);
		lblProjetos.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 1, 1));
		lblProjetos.setText(Messages.SelectImportServersPage_servers);
		new Label(container, SWT.NONE);
		new Label(container, SWT.NONE);

		cbOverwrite = new Button(container, SWT.CHECK);
		cbOverwrite.setEnabled(false);
		cbOverwrite.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 3, 1));
		cbOverwrite.setText(Messages.SelectImportServersPage_overwrite_dpl_servers);
		cbOverwrite.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				dialogChanged();
			}
		});
		checkboxTreeViewer = new CheckboxTreeViewer(container, SWT.BORDER);
		ServerViewContentProvider contentProvider = new ServerViewContentProvider();
		checkboxTreeViewer.setContentProvider(contentProvider);
		ILabelDecorator decorator = PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator();
		ImportServerDecoratingLabelProvider labelProvider = new ImportServerDecoratingLabelProvider();
		checkboxTreeViewer.setLabelProvider(new DecoratingLabelProvider(labelProvider, decorator));
		checkboxTreeViewer.setInput(new Object[] {});
		checkTreeViewerProjetos = checkboxTreeViewer.getTree();
		GridData gd_checkTreeViewerProjetos = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
		gd_checkTreeViewerProjetos.widthHint = 399;
		checkTreeViewerProjetos.setLayoutData(gd_checkTreeViewerProjetos);
		checkboxTreeViewer.addCheckStateListener(new ICheckStateListener() {
			// @Override
			@Override
			public void checkStateChanged(final CheckStateChangedEvent event) {
				IItemInfo nodeElement = (IItemInfo) event.getElement();
				boolean checked = event.getChecked();
				if (!(nodeElement instanceof IAppServerInfo)) {
					checkboxTreeViewer.setSubtreeChecked(nodeElement, checked);
				}
				Object[] checkedElements = checkboxTreeViewer.getCheckedElements();
				attributes.setSelectedItems(checkedElements);
				duplicatedItemsList = findDuplicated();
				dialogChanged();
			}
		});
		checkboxTreeViewer.addTreeListener(new ITreeViewerListener() {
			@Override
			public void treeCollapsed(final TreeExpansionEvent event) {
			}

			@Override
			public void treeExpanded(final TreeExpansionEvent event) {
				CheckboxTreeViewer viewer = (CheckboxTreeViewer) event.getSource();
				checkServersSelecionados(viewer.getTree().getItems());
			}
		});
		new Label(container, SWT.NONE);
		ExportTool.initializeServerStructure(serverTreeFactory);
		dialogChanged();
	}

	private void dialogChanged() {
		String message = null;
		Map<String, IItemInfo> map = getAttributes().getItemsSelected();
		map.clear();
		for (Object element : checkboxTreeViewer.getCheckedElements()) {
			if (element instanceof IAppServerInfo) {
				map.put(((IAppServerInfo) element).getName(), (IAppServerInfo) element);
			}
		}
		if (txtCaminhoDoPatch.getText().isEmpty()) { // $NON-NLS-1$
			message = Messages.SelectImportServersPage_source_file;
		} else if (map.size() == 0) {
			message = Messages.SelectImportServersPage_select_warning;
		}
		if (!getAttributes().getTargetFile().equalsIgnoreCase(txtCaminhoDoPatch.getText())) {
			getAttributes().setTargetFile(txtCaminhoDoPatch.getText());
			checkboxTreeViewer.refresh();
		}
		if (cbSelectTarget.getSelectionIndex() == -1) {
			message = Messages.SelectImportServersPage_select_target_node_tree;
		} else {
			IGroupInfo targetGroupInfo = (IGroupInfo) nodeList.get(cbSelectTarget.getSelectionIndex());
			getAttributes().setTargetNode(targetGroupInfo);
		}
		cbOverwrite.setEnabled(duplicatedItemsList.size() > 0);
		if (cbOverwrite.isEnabled() && !cbOverwrite.getSelection()) {
			message = Messages.SelectImportServersPage_duplicate_server_selecion_warning;
		}
		updateStatus(message);
	}

	private void fillComboOptions() {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		final IGroupInfo root = serverManager.getItems();
		cbSelectTarget.clearSelection();
		cbSelectTarget.removeAll();
		nodeList = new ArrayList<IItemInfo>();
		readNodeTreeView(root, 0);
	}

	protected List<IItemInfo> findDuplicated() {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();

		Object[] selectedItems = attributes.getSelectedItems();
		duplicatedItemsList.clear();
		duplicatedItemsList = ImportTools.findDuplicatedItems(selectedItems, serverManager);

		return duplicatedItemsList;
	}

	public ServerImporExportAttributesVO getAttributes() {
		return attributes;
	}

	public Button getBtnAbrirDialog() {
		return btnSelectFile;
	}

	public Tree getCheckTreeViewerProjetos() {
		return checkTreeViewerProjetos;
	}

	public Text getTxtCaminhoDoPatch() {
		return txtCaminhoDoPatch;
	}

	private void openDialogFile() {
		String filePath = null;
		FileDialog dialog = new FileDialog(shell);
		dialog.setText(Messages.SelectImportServersPage_target);
		dialog.setFilterNames(new String[] { Messages.SelectImportServersPage_server_extension_file });
		dialog.setFilterExtensions(new String[] { "*.srv" }); //$NON-NLS-1$
		dialog.setFilterPath("/"); //$NON-NLS-1$
		filePath = dialog.open();
		if (filePath != null) {
			txtCaminhoDoPatch.setText(changeExtension(filePath, "*.srv")); // Qual a finalidade? Alan //$NON-NLS-1$
			XMLServerRoot xmlServerRoot = null;
			try {
				xmlServerRoot = readFile(filePath);
				verifyXMLVersion(xmlServerRoot);
				updateInput(xmlServerRoot);
				dialogChanged();
			} catch (IOException e) {
				e.printStackTrace();
				String exceptionMessage = e.getMessage();
				if (exceptionMessage == null) {
					exceptionMessage = Messages.SelectImportServersPage_file_read_error;
				}
				System.out.println("SelectImportServersPage.openDialogFile()"); //$NON-NLS-1$
//				UiUtils.openMessageDialog(new Shell(Display.getDefault()), Messages.SelectImportServersPage_11, exceptionMessage,
//						MessageDialog.ERROR, new String[] { Messages.SelectImportServersPage_12 });
			}
		}
	}

	private XMLServerRoot readFile(final String filePath) {
		return ImportTools.importXml(filePath);
	}

	private void readNodeTreeView(final IGroupInfo element, int level) {
		if (level == 0) {
			cbSelectTarget.add(element.getName());
		} else {
			cbSelectTarget.add(String.format("%-" + (level * 2) + "s%s", " ", element.getName())); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		}
		nodeList.add(element);
		level++;
		for (int i = 0; i < element.getChildren().size(); i++) {
			IItemInfo group = element.getChildren().get(i);
			if (group instanceof IGroupInfo) {
				readNodeTreeView((IGroupInfo) group, level);
			}
		}
		level--;
		cbSelectTarget.select(0);
	}

	public void setAttributes(final ServerImporExportAttributesVO attributes) {
		this.attributes = attributes;
		checkServersSelecionados(checkTreeViewerProjetos.getItems());
	}

	private void updateInput(final XMLServerRoot serverTree) {
		if (serverTree == null) {
			return;
		}
		attributes.setXMLServerRoot(serverTree);
		Group serverTreeRoot = serverTree.getServerTreeRoot();
		IGroupInfo groupInfo = ImportTools.toGroupInfo(serverTreeRoot);
		Object[] input = (Object[]) checkboxTreeViewer.getInput();
		if (input == null || input.length == 0) {
			input = new Object[] { groupInfo };
			checkboxTreeViewer.setInput(input);
		} else {
			input[0] = groupInfo;
		}
		checkboxTreeViewer.refresh();
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();

		ImportTools.findDuplicatedItems(groupInfo.getChildren().toArray(), serverManager);
		IDecoratorManager decoratorManager = PlatformUI.getWorkbench().getDecoratorManager();
		String decoratorId = "br.com.totvs.tds.ui.server.views.decorator.importServer.duplicated"; //$NON-NLS-1$
		decoratorManager.update(decoratorId);
	}

	private void updateStatus(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

	private void verifyXMLVersion(final XMLServerRoot xmlServerRoot) throws IOException {
		if (xmlServerRoot.getVersion() == null) {
			throw new IOException(Messages.SelectImportServersPage_version_error);
		}
		if (!xmlServerRoot.getVersion().equals(XMLVersionControl.V_11_3_8)) {
			throw new IOException(String.format(Messages.SelectImportServersPage_invalid_version_error,
					xmlServerRoot.getVersion(), XMLVersionControl.V_11_3_8));
		}
	}

}
