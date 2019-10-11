package br.com.totvs.tds.ui.server.wizards.patch;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.dialogs.WizardExportResourcesPage;

import br.com.totvs.tds.server.jobs.BuildPatchAttributes;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.ServerUIIcons;

/**
 * Geração de pacote a partir da área de trabalho.
 */
public class PatchWorkareaPage extends WizardExportResourcesPage {

	private BuildPatchAttributes attributes;
	private Button compileCheckbox;
	private Button verifyRpo;

	public PatchWorkareaPage(BuildPatchAttributes attributes) { // org.eclipse.jface.viewers.IStructuredSelection
		super("patchWorkareaPage", attributes.geSelection());

		this.attributes = attributes;

		setTitle("Geração a partir da Área de Trabalho");
		setDescription(
				"Este assistente o auxiliará na geração do pacote a partir de seleção efetuada na Área de Trabalho.");
		setImageDescriptor(ServerUIIcons.getBuildPatch());

	}

	@Override
	protected void createDestinationGroup(org.eclipse.swt.widgets.Composite parent) {
		// neste contexto não é necessário grupo de destino, pois já foi definido na
		// página incial ()
	}

	/**
	 * Creates the build options group controls.
	 *
	 * @param optionsGroup the parent control
	 */
	@Override
	protected void createOptionsGroupButtons(Group optionsGroup) {
		compileCheckbox = new Button(optionsGroup, SWT.CHECK | SWT.LEFT);
		compileCheckbox.setText("Recompilar recursos antes de gerar o pacote.");
		compileCheckbox.setFont(optionsGroup.getFont());
		compileCheckbox.addListener(SWT.Selection, this);

		verifyRpo = new Button(optionsGroup, SWT.CHECK | SWT.LEFT);
		verifyRpo.setText("Verificar no RPO.");
		verifyRpo.setFont(optionsGroup.getFont());
		verifyRpo.addListener(SWT.Selection, this);
	}

	/**
	 * @return <code>true</code> if this page is complete, and <code>false</code> if
	 *         incomplete
	 * @see #validateSourceGroup
	 * @see #validateOptionsGroup
	 */
	@Override
	protected boolean determinePageCompletion() {
		boolean complete = validateSourceGroup();

		if (complete) {
			setErrorMessage(null);
			attributes.setResourcesFiles(getFiles(getWhiteCheckedResources()));
		} else {
			setErrorMessage("There are no resources currently selected for process.");
		}

		return complete;
	}

	private List<IFile> getFile(IContainer container) {
		List<IFile> result = new ArrayList<IFile>();

		try {
			for (Object element : container.members()) {
				if (element instanceof IContainer) {
					result.addAll(getFile((IContainer) element));
				} else if (element instanceof IFile) {
					result.add(((IFile) element));
				}
			}
		} catch (CoreException e) {
			setErrorMessage(e.getMessage());
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}

		return result;
	}

	private List<IFile> getFiles(List resources) {
		List<IFile> result = new ArrayList<IFile>();

		for (Object element : resources) {
			if (element instanceof IContainer) {
				result.addAll(getFile((IContainer) element));
			} else if (element instanceof IFile) {
				result.add(((IFile) element));
			}
		}

		return result;
	}

	@Override
	public IWizardPage getNextPage() {
		return null;
	}

	@Override
	public void handleEvent(Event event) {
		Widget source = event.widget;

		if (source == compileCheckbox) {
			attributes.setCompile(compileCheckbox.getSelection());
		} else if (source == verifyRpo) {
			attributes.setVerifyFilesRPO(verifyRpo.getSelection());
		}

		updatePageCompletion();
	}

	/**
	 * Returns whether the extension of the given resource name is an extension that
	 * has been specified for export by the user.
	 *
	 * @param resourceName the resource name
	 * @return <code>true</code> if the resource name is suitable for export based
	 *         upon its extension
	 */
	@Override
	protected boolean hasExportableExtension(String resourceName) {
		boolean result = true;

		int separatorIndex = resourceName.lastIndexOf('.');
		if (separatorIndex < 1) {
			return false;
		}

		return result && super.hasExportableExtension(resourceName);
	}

}
