package br.com.totvs.tds.ui.server.wizards.patch;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.jobs.BuildPatchAttributes;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryServerNode;
import br.com.totvs.tds.ui.server.fileSystem.ServerFileSystemFactory;
import br.com.totvs.tds.ui.server.widget.IItemSelectionChangedListener;
import br.com.totvs.tds.ui.server.widget.ServerDirectoryTreeViewer;

/**
 * Página de geração de patchs por comparação.
 */
public class PatchComparisonPage extends WizardPage {

	private ServerDirectoryTreeViewer viewer;

	private final BuildPatchAttributes attributes;
	private Text txtRPOMaster;

	/**
	 * Create the wizard.
	 *
	 * @param attributes
	 */
	public PatchComparisonPage(final BuildPatchAttributes attributes) {
		super("patchComparisonPage"); //$NON-NLS-1$

		setTitle(Messages.PatchComparisonPage_Generation_by_comparison);
		setDescription(Messages.PatchComparisonPage_Wizard_assist_you_generating_patch_by_comparison);

		this.attributes = attributes;
	}

	private void addWizardContainerSelectionPageListener(final IPageChangeProvider wizardContainer) {
		wizardContainer.addPageChangedListener(new IPageChangedListener() {
			@Override
			public void pageChanged(final PageChangedEvent event) {
				if (event.getSelectedPage() instanceof PatchComparisonPage) {
					IAppServerInfo server = attributes.getServer();
					String environment = attributes.getEnvironment();

					if (server != null && environment != null) {
						loadFolderStructure();
					}
				}
			}
		});
	}

	/**
	 * Create contents of the wizard.
	 *
	 * @param parent
	 */
	@Override
	public void createControl(final Composite parent) {
		addWizardContainerSelectionPageListener((IPageChangeProvider) this.getContainer());

		Composite container = new Composite(parent, SWT.NONE);

		setTitle(Messages.PatchComparisonPage_Generation_by_comparison_title);
		setControl(container);
		container.setLayout(new GridLayout(2, false));

		viewer = new ServerDirectoryTreeViewer(container, false);
		Tree tree = viewer.getTree();
		tree.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));
		viewer.addItemSelectionChangedListener(new IItemSelectionChangedListener() {
			@Override
			public void selectionChanged() {
				attributes.setMasterPatch(viewer.getItem().getAbsolutPath());
				txtRPOMaster.setText(attributes.getMasterPatch());
				dialogChanged();
			}
		});

		Label lblLocalRpoMestre = new Label(container, SWT.NONE);
		lblLocalRpoMestre.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1));
		lblLocalRpoMestre.setText(Messages.PatchComparisonPage_Master_rpo);

		txtRPOMaster = new Text(container, SWT.BORDER);
		txtRPOMaster.setEditable(false);
		txtRPOMaster.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));
	}

	private void dialogChanged() {
		if ((attributes.getMasterPatch() == null) || (attributes.getMasterPatch().length() == 0)) {
			updateStatus(Messages.PatchComparisonPage_Master_rpo_error);
			return;
		}

		updateStatus(null);
	}

	@Override
	public IWizardPage getNextPage() {
		return null;
	}

	protected void loadFolderStructure() {
		setErrorMessage(null);

		try {
			getContainer().run(true, true, new IRunnableWithProgress() {
				@Override
				public void run(final IProgressMonitor monitor) {
					try {
						monitor.beginTask("Lendo estrutura de pastas", IProgressMonitor.UNKNOWN);

						IServerDirectoryServerNode serverNode = ServerFileSystemFactory
								.createServerNode(attributes.getServer(), attributes.getEnvironment(), true);
						viewer.addServerDirectoryNode(serverNode, true);
					} finally {
						Display.getDefault().asyncExec(new Runnable() {

							@Override
							public void run() {
								viewer.refresh();
							}
						});
					}
				}
			});
		} catch (OperationCanceledException e) {
			setErrorMessage("Carga da estrura de pastas cancelada.");
		} catch (InvocationTargetException e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		} catch (InterruptedException e) {
			ServerUIActivator.logStatus(IStatus.CANCEL, e.getMessage());
		}
	}

	private void updateStatus(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

}
