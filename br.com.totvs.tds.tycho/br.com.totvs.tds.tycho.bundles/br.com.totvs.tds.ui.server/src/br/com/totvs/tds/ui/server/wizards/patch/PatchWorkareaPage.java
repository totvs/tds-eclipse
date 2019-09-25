package br.com.totvs.tds.ui.server.wizards.patch;

import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.ITreeViewerListener;
import org.eclipse.jface.viewers.TreeExpansionEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;

import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.server.jobs.BuildPatchAttributes;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.ServerUIIcons;

/**
 * Geração de pacote a partir da �rea de trabalho.
 */
public class PatchWorkareaPage extends WizardPage {

	private final BuildPatchAttributes attributes;
	private CheckboxTreeViewer treeList;
	private boolean lShowFiles;
	private final Set<IResource> projects = new HashSet<IResource>();
	private Button btnCompile;

	/**
	 * Create the wizard.
	 *
	 * @param attributes
	 */
	public PatchWorkareaPage(final BuildPatchAttributes attributes) {
		super("PatchWorkareaPage"); //$NON-NLS-1$
		setImageDescriptor(ServerUIIcons.getBuildPatch());

		setTitle("Geração a partir da �rea de trabalho");
		setDescription("A geração do pacote de Atualização levar� em consideração os c�digos que estão no RPO");
		this.attributes = attributes;
	}

	private void addResourceError(final IResource resourceRoot) {
		IResource resource = resourceRoot;
		IMarker[] erros;
		try {
			erros = resource.findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);

			if (erros.length > 0) {
				for (IMarker iMarker : erros) {
					if ((Integer) (iMarker.getAttribute(IMarker.SEVERITY)) == IMarker.SEVERITY_ERROR) {
						projects.add(resource);
					}
				}
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}

	private void addTrackPathSelectionToTree(final Tree tree) {
		tree.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseDown(final MouseEvent event) {
				Point point = new Point(event.x, event.y);
				TreeItem item = tree.getItem(point);
				if (item != null) {
					boolean checked = tree.getItem(point).getChecked();
//					UiUtils.checkItems(item, checked);
//					UiUtils.checkPath(item.getParentItem(), checked, false);
				}
			}
		});
	}

	private void addWizardContainerSelectionPageListener(final IPageChangeProvider wizardContainer) {
//		wizardContainer.addPageChangedListener(new IPageChangedListener() {
//			@Override
//			public void pageChanged(final PageChangedEvent event) {
//				if (event.getSelectedPage() instanceof PatchWorkareaPage) {
//					((List<?>) attributes.getAdapter(ISelectedList.class)).clear();
//					invisibleRoot.clear();
//					IWorkspace work = ResourcesPlugin.getWorkspace();
//					IProject[] projects = work.getRoot().getProjects();
//
//					ResourceItemNode node;
//					for (IProject project : projects) {
//						if (project.isOpen()) {
//							boolean hasNature = false;
//							try {
//								hasNature = project.hasNature(TotvsNature.NATURE_ID);
//							} catch (CoreException e1) {
//								e1.printStackTrace();
//							}
//							if (hasNature) {
//								node = new ResourceItemNode(project.getName(), ItemDirectType.PROJECT);
//								node.setResource(project);
//								invisibleRoot.add(node);
//
//								if (getShowFiles()) {
//									try {
//										IResource[] resources = project.members();
//										for (IResource iResource : resources) {
//											loadProjectResouces(iResource, node);
//										}
//									} catch (CoreException e) {
//										e.printStackTrace();
//									}
//								}
//							}
//						}
//					}
//
//					treeList.refresh();
//					// if (attributes.getCheckedResources().isEmpty()) {
//					// treeList.collapseAll();
//					// } else {
//					// treeList.expandAll();
//					// }
//					dialogChanged();
//				}
//			}
//		});
	}

	private boolean containsExtensionInvalid(final String resourceName) {
		String aux = resourceName.toLowerCase();
		if (aux.contains(".err") || aux.contains(".erx_") || aux.contains(".ppx")) { //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			return true;
		} else if (aux.endsWith(".pdb")) { //$NON-NLS-1$
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Create contents of the wizard.
	 *
	 * @param parent
	 */
	@Override
	public void createControl(final Composite parent) {
		addWizardContainerSelectionPageListener((IPageChangeProvider) this.getContainer());

		Composite container = new Composite(parent, SWT.NULL);
		setControl(container);

		container.setLayout(new GridLayout(3, false));

		Label lblElementosSelecionados = new Label(container, SWT.NONE);
		lblElementosSelecionados.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 3, 1));
		lblElementosSelecionados.setText("Recursos selecionados");

		treeList = new CheckboxTreeViewer(container);
//		treeList.setContentProvider(new ResourceContentProvider());
//		treeList.setLabelProvider(new ResourceLabelProvider());
		treeList.getTree().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1));
//		treeList.setInput(invisibleRoot);

		treeList.setComparator(new ViewerComparator() {
			@Override
			public int compare(final Viewer viewer, final Object e1, final Object e2) {
				return e1.toString().compareToIgnoreCase(e2.toString());
			}
		});

		// REFERENCE: TREE - Adiciona a Tree informada, a funcionalidade que
		// pinta os Itens pais, indicando o caminho do TreeItem selecionado
		addTrackPathSelectionToTree(treeList.getTree());
		treeList.addCheckStateListener(new ICheckStateListener() {
			@Override
			public void checkStateChanged(final CheckStateChangedEvent event) {
				// ResourceItemNode element = (ResourceItemNode) event.getElement();
				// setCheckResourceNode(event.getChecked(), element);

				dialogChanged();
				treeList.refresh();
			}
		});

		Button btnLimparSelecao = new Button(container, SWT.NONE);
		btnLimparSelecao.setToolTipText("Limpa as seleções efetuadas");
		btnLimparSelecao.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		btnLimparSelecao.setText("Limpar seleção");
		btnLimparSelecao.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
//				((List<?>) attributes.getAdapter(ISelectedList.class)).clear();
				treeList.setAllChecked(false);
				dialogChanged();
			}
		});

		btnCompile = new Button(container, SWT.CHECK);
		btnCompile.setToolTipText("Compilar selecionados na geração do pacote");
		btnCompile.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		btnCompile.setText("Compilar selecionados");
		btnCompile.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				attributes.setCompile(btnCompile.getSelection());
				dialogChanged();
			}
		});

		final Button btnVerifyFilesRPO = new Button(container, SWT.CHECK);
		IPreferenceStore store = ServerUIActivator.getDefault().getPreferenceStore();
		boolean isVerify = store.getBoolean(IServerConstants.VERIFY_RPO);
		btnVerifyFilesRPO.setSelection(isVerify);
		attributes.setVerifyFilesRPO(isVerify);
		btnVerifyFilesRPO.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false, 1, 1));
		btnVerifyFilesRPO.setText("Verificar arquivos no RPO");
		btnVerifyFilesRPO.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(final SelectionEvent e) {
				attributes.setVerifyFilesRPO(btnVerifyFilesRPO.getSelection());
			}
		});

		treeList.addTreeListener(new ITreeViewerListener() {
			@Override
			public void treeCollapsed(final TreeExpansionEvent event) {
			}

			@Override
			public void treeExpanded(final TreeExpansionEvent event) {
//				ResourceItemNode element = (ResourceItemNode) event.getElement();
//				setCheckResourceNode(treeList.getChecked(element), element);
				dialogChanged();
			}
		});
		setPageComplete(false);
	}

	private void dialogChanged() {
		if (attributes.getResources().size() == 0) {
			updateStatus("Selecione um ou mais projetos.");
			return;
		}

		if (btnCompile.getSelection()) {
			updateStatus(null);
			setMessage(
					"O recurso selecionado atualmente cont�m erro. Caso persista o erro ap�s a compilação, não ser� gerado o patch.",
					WARNING);
		} else {
			updateStatus(
					"O recurso selecionado cont�m erro, verifique detalhes no console. Use 'Compilar selecionados' para recompilar.");

			if (projects.size() > 0) {
//					TdsLogging.getDefault().logInformation(Messages.PatchWorkareaPage_15);

				for (IResource resource : projects) {
					if (resource.getType() == (IResource.FILE)) {
//							TdsLogging.getDefault()
//									.logError(String.format(Messages.PatchWorkareaPage_16, resource.getFullPath()));
					}
					if (resource.getType() == IResource.PROJECT) {
//							TdsLogging.getDefault()
//									.logError(String.format(Messages.PatchWorkareaPage_17, resource.getFullPath()));
					}
				}
				projects.clear();
			}

			return;
		}

		updateStatus(null);
	}

	@Override
	public IWizardPage getNextPage() {
		IWizardPage nextPage = null;

		return nextPage;
	}

	public boolean getShowFiles() {
		return lShowFiles;
	}

	private void loadProjectResouces(final IResource resource, final IResource nodeRoot) throws CoreException {
		String resourceName = resource.getName();
		// Alteração feita referente ao chamado : TDS-6121
		if (resourceName.startsWith(".") || containsExtensionInvalid(resourceName)) { //$NON-NLS-1$
			return;
		}
		boolean isInResourceList = false; // attributes.containsResourceFullPath(resource.getFullPath().toOSString());
		if (resource.getType() == IResource.FILE) {
//			ResourceItemNode node = new ResourceItemNode(resource.getName(), ItemDirectType.FILE);
//			nodeRoot.add(node);
//			node.setResource((IFile) resource);
//			node.setType(ItemDirectType.FILE);
			if (isInResourceList) {
//				node.setChecked(true);
//				setCheckResourceNode(true, node);
			}
		} // else if (resource.getType() == IResource.FOLDER) {
			// verifica se esta pasta não deve ser compilada
			// IResourceWrapper wrapper = WrapperManager.getInstance().getWrapper(resource);
//			if (!wrapper.isIgnoreCompile()) {
//				IFolder folder = (IFolder) resource;
//				ResourceItemNode node = new ResourceItemNode(resource.getName(), ItemDirectType.FOLDER);
//				node.setResource(folder);
//				node.setType(ItemDirectType.FOLDER);
//				node.setChecked(isInResourceList);
//				if (isInResourceList) {
//					// node.setChecked(true);
//					setCheckResourceNode(true, node);
//				}
//				IResource[] resources = folder.members(IFolder.EXCLUDE_DERIVED);
//				if (resources.length > 0) {
//					nodeRoot.add(node);
//					for (IResource iResource : resources) {
//						loadProjectResouces(iResource, node);
//					}
//				}
//			}
//		}
	}

	/**
	 * Adiciona o recurso selecionado e os filhos a uma lista e marca setChecked()
	 * como true ou false.
	 */
	private void setCheckResourceNode(final boolean checked, final IResource resourceRoot) {
		treeList.setChecked(resourceRoot, checked);
		IResource resource = resourceRoot;
		if (attributes.getResources().contains(resource)) {
			if (!checked) {
				// attributes.getResources().remove(resource);
				// attributes.removeResource(resource);
			}
			// treeList.setChecked(resourceRoot, true);
			// attributes.getResources().add(resourceRoot.getResource());
			// addResourceError(resourceRoot);
		}

//		if (resourceRoot.getType().equals(ItemDirectType.FILE)) {
//
//			if (checked) {
//				if (!(attributes.getResources().contains(resource))) {
//					// Adiciona os arquivos selecionados a uma lista.
//					// attributes.getResources().add(resource);
//					attributes.addResource(resource);
//					addResourceError(resourceRoot);
//				}
//
//			} else {
//				// Remove os arquivos que não estão selecionados da lista.
//				((List<?>) attributes.getAdapter(ISelectedList.class)).remove(resource);
//			}
//		} else if (resourceRoot.getType().equals(ItemDirectType.FOLDER)
//				|| resourceRoot.getType().equals(ItemDirectType.PROJECT)) {
//			if (attributes.getProcesso().equals(BuildPatchProcessType.BY_WORKAREA)
//					&& resourceRoot.getType().equals(ItemDirectType.PROJECT)) {
//				addResourceError(resourceRoot);
//			}
//
//			for (ResourceItemNode children : resourceRoot.getChildren()) {
//				// Seta o true ou false nos filhos.
//				setCheckResourceNode(checked, children);
//			}
//		}
	}

	public void setShowFiles(final boolean lShowFiles) {
		this.lShowFiles = lShowFiles;
	}

	private void updateStatus(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

}
