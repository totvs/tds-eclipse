// FIXME: Utilizar o provider da Visão "Servidores" - TDS-5842
package br.com.totvs.tds.ui.server.wizards;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.tools.ExportTool;
import br.com.totvs.tds.server.xml.Group;
import br.com.totvs.tds.server.xml.ObjectFactory;
import br.com.totvs.tds.server.xml.XMLServerRoot;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.views.ServerViewContentProvider;
import br.com.totvs.tds.ui.server.views.ServerViewLabelProvider;
import br.com.totvs.tds.ui.server.vo.ServerImporExportAttributesVO;

/**
 * P�gina de seleção para exportação de servidores.
 *
 * @author acandido
 */
public class SelectExportServersPage extends WizardPage {

	private ServerImporExportAttributesVO attributes;
	private Button btnAbrirDialog;
	private CheckboxTreeViewer checkboxTreeViewer;
	private Tree checkTreeViewerProjetos;
	private Label lblDestino;
	private final ObjectFactory objectFactory = new ObjectFactory();
	private Shell shell;
	private Text txtCaminhoDoPatch;
	private XMLServerRoot xmlServerRoot = null;

	/**
	 * Create the wizard.
	 *
	 * @param expAttributes
	 */
	public SelectExportServersPage(final ServerImporExportAttributesVO expAttributes) {
		super("selectExportServersView"); //$NON-NLS-1$
		setTitle(Messages.SelectExportServersPage_selection);
		setDescription(Messages.SelectExportServersPage_selection_warnig);
		attributes = expAttributes;
	}

	private Map<String, IItemInfo> addServerStructure(final Object[] checkedElements) {
		Map<String, IItemInfo> map = new HashMap<String, IItemInfo>();
		for (Object element : checkboxTreeViewer.getCheckedElements()) {
			if (element instanceof IItemInfo) {
				IItemInfo item = (IItemInfo) element;
				item.setPersistentProperty(IItemInfo.PROPERTY_ISSELECTED, true);
				if (item instanceof IGroupInfo && item.getParent() == null
						&& !map.containsKey(IItemInfo.SERVERS_ROOT)) {
					map.put(IItemInfo.SERVERS_ROOT, item);
				} else {
					map.put(item.getName(), item);
				}
			}
		}
		return map;
	}

	private String changeExtension(final String fOriginalName, final String fNewExtension) {
		String newExtension = fNewExtension;
		int lastDot = fOriginalName.lastIndexOf("."); //$NON-NLS-1$
		if (newExtension.startsWith("*.")) { //$NON-NLS-1$
			newExtension = newExtension.substring(1);
		}
		if (lastDot != -1) {
			return fOriginalName.substring(0, lastDot) + newExtension;
		}

		return fOriginalName + newExtension;
	}

	private void checkServersSelecionados(final TreeItem[] items) {
		Map<String, IItemInfo> mapFontes = attributes.getItemsSelected();
		for (int i = 0; i < items.length; i++) {
			TreeItem item = items[i];

			if (mapFontes.containsValue(item.getText())) {
				item.setChecked(true);
				System.out.println("SelectExportServersPage.checkServersSelecionados()"); //$NON-NLS-1$
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
		Label lblProjetos = new Label(container, SWT.NONE);
		lblProjetos.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false, 1, 1));
		lblProjetos.setText(Messages.SelectExportServersPage_servers);
		new Label(container, SWT.NONE);
		new Label(container, SWT.NONE);
		checkboxTreeViewer = new CheckboxTreeViewer(container, SWT.BORDER);
		checkboxTreeViewer.setContentProvider(new ServerViewContentProvider());
		checkboxTreeViewer.setLabelProvider(new ServerViewLabelProvider());
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		IGroupInfo items = serverManager.getItems();
		setAllItemsNotSelected(items);
		checkboxTreeViewer.setInput(new Object[] { items });
		checkboxTreeViewer.addCheckStateListener(new ICheckStateListener() {
			@Override
			public void checkStateChanged(final CheckStateChangedEvent event) {
				IItemInfo nodeElement = (IItemInfo) event.getElement();
				boolean isChecked = event.getChecked();
				Group serverTreeRoot = xmlServerRoot.getServerTreeRoot();
				if (isChecked) {
					System.out.println(
							"SelectExportServersPage.createControl(...).new ICheckStateListener() {...}.checkStateChanged()"); //$NON-NLS-1$
//					ServerUIUtil.checkParents(nodeElement, objectFactory, serverTreeRoot, checkboxTreeViewer);
//					ServerUIUtil.checkChildren(nodeElement, objectFactory, serverTreeRoot, checkboxTreeViewer);
				} else {
//					ServerUIUtil.unCheckParents(nodeElement, serverTreeRoot, checkboxTreeViewer);
				}
				if (!(nodeElement instanceof IAppServerInfo)) {
					checkboxTreeViewer.setSubtreeChecked(nodeElement, event.getChecked());
				}
				dialogChanged();
			}
		});
		checkTreeViewerProjetos = checkboxTreeViewer.getTree();
		checkTreeViewerProjetos.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));
		new Label(container, SWT.NONE);
		lblDestino = new Label(container, SWT.NONE);
		lblDestino.setText(Messages.SelectExportServersPage_target);

		Composite ctr = new Composite(container, SWT.NONE);
		ctr.setLayout(new GridLayout(2, false));
		ctr.setLayoutData(new GridData(SWT.FILL, SWT.NONE, true, false));

		txtCaminhoDoPatch = new Text(ctr, SWT.BORDER | SWT.READ_ONLY);
		txtCaminhoDoPatch.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 1, 1));
		txtCaminhoDoPatch.addKeyListener(new KeyAdapter() {
			@Override
			public void keyReleased(final KeyEvent e) {
				dialogChanged();
			}
		});
		txtCaminhoDoPatch.setEditable(true);
		Text txtSrv = new Text(ctr, SWT.BORDER | SWT.READ_ONLY);
		txtSrv.setText(Messages.SelectExportServersPage_server_extension_file);
		txtSrv.setEnabled(false);
		txtSrv.setEditable(false);
		btnAbrirDialog = new Button(container, SWT.NONE);
		btnAbrirDialog.setLayoutData(new GridData(SWT.RIGHT, SWT.CENTER, false, false, 1, 1));
		btnAbrirDialog.addSelectionListener(new SelectionAdapter() {
			// ////@Override
			@Override
			public void widgetSelected(final SelectionEvent e) {
				openDialogFile();
			}
		});
		btnAbrirDialog.setText("..."); //$NON-NLS-1$
		xmlServerRoot = ExportTool.initializeServerStructure(objectFactory);
		attributes.setXMLServerRoot(xmlServerRoot);
		dialogChanged();
	}

	private void dialogChanged() {
		String message = null;
		Map<String, IItemInfo> map = getAttributes().getItemsSelected();
		map.clear();
		map = addServerStructure(checkboxTreeViewer.getCheckedElements());
		if (map.size() == 0) {
			message = Messages.SelectExportServersPage_select_one_or_more_servers_warning;
		} else if ("".equals(txtCaminhoDoPatch.getText())) { //$NON-NLS-1$
			message = Messages.SelectExportServersPage_target_file;
		} else if (!txtCaminhoDoPatch.getText().endsWith(".srv")) { //$NON-NLS-1$
			message = Messages.SelectExportServersPage_target_file_warning;
		}
		getAttributes().setTargetFile(txtCaminhoDoPatch.getText());
		getAttributes().getItemsSelected().putAll(map);
		updateStatus(message);
	}

	public ServerImporExportAttributesVO getAttributes() {
		return attributes;
	}

	public Button getBtnAbrirDialog() {
		return btnAbrirDialog;
	}

	public Tree getCheckTreeViewerProjetos() {
		return checkTreeViewerProjetos;
	}

	public Text getTxtCaminhoDoPatch() {
		return txtCaminhoDoPatch;
	}

	private void openDialogFile() {
		String pacPath = null;
		FileDialog dialog = new FileDialog(shell);
		dialog.setText(Messages.SelectExportServersPage_target);

		// TODO salvar para posterior recuperação
		// dialog.setFilterPath("C:/"); //$NON-NLS-1$
		dialog.setFilterNames(new String[] { Messages.SelectExportServersPage_server_extension_file });
		dialog.setFilterExtensions(new String[] { "*.srv" }); //$NON-NLS-1$
		pacPath = dialog.open();

		if (pacPath != null) {
			txtCaminhoDoPatch.setText(changeExtension(pacPath, "*.srv")); //$NON-NLS-1$
		}
		dialogChanged();
	}

	private void setAllItemsNotSelected(final IGroupInfo items) {
		items.setPersistentProperty(IItemInfo.PROPERTY_ISSELECTED, false);
		List<IItemInfo> children = items.getChildren();
		if (children != null && children.size() > 0) {
			for (IItemInfo itemInfo : children) {
				if (itemInfo instanceof IGroupInfo) {
					IGroupInfo pItem = (IGroupInfo) itemInfo;
					setAllItemsNotSelected(pItem);
				}
			}
		}
	}

	public void setAttributes(final ServerImporExportAttributesVO attributes) {
		this.attributes = attributes;
		checkServersSelecionados(checkTreeViewerProjetos.getItems());
	}

	private void updateStatus(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

}
