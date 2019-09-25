package br.com.totvs.tds.ui.server.wizards.patch;

import org.eclipse.jface.dialogs.IPageChangeProvider;
import org.eclipse.jface.dialogs.IPageChangedListener;
import org.eclipse.jface.dialogs.PageChangedEvent;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;

import br.com.totvs.tds.server.jobs.BuildPatchAttributes;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryServerNode;
import br.com.totvs.tds.ui.server.fileSystem.ServerFileSystemFactory;
import br.com.totvs.tds.ui.server.widget.IItemSelectionChangedListener;
import br.com.totvs.tds.ui.server.widget.ServerDirectoryTreeViewer;

/**
 * P�gina de geração de patchs por comparação.
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
		super("PatchComparisonPage"); //$NON-NLS-1$

		setTitle("Geração por comparação");
		setDescription("Este assistente o auxiliara na geração do pacote de Atualização por comparação.");

		this.attributes = attributes;
	}

	private void addWizardContainerSelectionPageListener(final IPageChangeProvider wizardContainer) {
		wizardContainer.addPageChangedListener(new IPageChangedListener() {
			@Override
			public void pageChanged(final PageChangedEvent event) {
				if (event.getSelectedPage() instanceof PatchComparisonPage) {
					IServerDirectoryServerNode serverNode = ServerFileSystemFactory.getInstance()
							.createServerNode(attributes.getServer(), attributes.getEnvironment(), true);
					viewer.addServerDirectoryNode(serverNode);
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

		setTitle("Geração de pacote por comparação");
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
		lblLocalRpoMestre.setText("Local RPO mestre");

		txtRPOMaster = new Text(container, SWT.BORDER);
		txtRPOMaster.setEditable(false);
		txtRPOMaster.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 1, 1));

		dialogChanged();
	}

	private void dialogChanged() {
		if ((attributes.getMasterPatch() == null) || (attributes.getMasterPatch().length() == 0)) {
			updateStatus("Selecione o local que contem o RPO mestre.");
			return;
		}
		updateStatus(null);
	}

	@Override
	public IWizardPage getNextPage() {
		return null;
	}

	private void updateStatus(final String message) {
		setErrorMessage(message);
		setPageComplete(message == null);
	}

}
