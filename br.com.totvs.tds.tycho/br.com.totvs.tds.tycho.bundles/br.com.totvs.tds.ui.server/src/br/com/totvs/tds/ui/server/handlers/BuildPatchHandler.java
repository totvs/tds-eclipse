package br.com.totvs.tds.ui.server.handlers;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.jobs.BuildPatchAttributes;
import br.com.totvs.tds.server.jobs.BuildPatchProcessType;
import br.com.totvs.tds.ui.server.wizards.BuildPatchWizard;

/**
 * Handler para geração de um patch.
 */
public class BuildPatchHandler extends ServerHandler {

	private BuildPatchAttributes attributes = null;
	private BuildPatchWizard patchWizard;

	@SuppressWarnings("unchecked")
	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		IAppServerInfo currentServer = serverManager.getCurrentServer();

		ISelection select = HandlerUtil.getCurrentSelection(event);
		if (select instanceof IStructuredSelection) {
			attributes = new BuildPatchAttributes();
			patchWizard = new BuildPatchWizard(attributes);

			IStructuredSelection selection = (IStructuredSelection) select;

			Object[] res = selection.toArray();

			if (res != null && res.length > 0) {
				if (res[0] instanceof IContainer) {
					List<String> resources = new ArrayList<String>();
					attributes.setProcesso(BuildPatchProcessType.BY_WORKAREA);
					getResources(selection.toList(), resources);
					attributes.setResources(resources);
				} else {
					attributes.setProcesso(BuildPatchProcessType.BY_RPO);
				}
			}
		} else {
			attributes.setProcesso(BuildPatchProcessType.BY_RPO);
			patchWizard = new BuildPatchWizard(attributes);
		}

		try {
			if (currentServer != null) {
				Shell shell = new Shell(Display.getCurrent());
				WizardDialog dialog = new WizardDialog(shell, patchWizard);
				dialog.setBlockOnOpen(true);
				dialog.setHelpAvailable(true);
				dialog.open();
			} else {
				throw new Exception("Nenhum servidor ativo encontrado para utilização desta funcionalidade.");
			}
		} catch (Exception e) {
			e.printStackTrace();
			// String msg = String.format(Messages.BuildPatchHandler_1, e.getMessage());
			// TdsLogging.getDefault().showError(SdkActivator.PLUGIN_ID, msg);
		}
		//
		return null;
	}

	protected void getResources(final IContainer container, final List<String> resources) {
		try {
			for (IResource resource : container.members()) {
				if (resource instanceof IContainer) {
					getResources(((IContainer) resource), resources);
				} else {
					resources.add(resource.getFullPath().toPortableString());
				}
			}
		} catch (CoreException e) {
			ServerActivator.logStatus(IStatus.ERROR, "Geração de Pacote", e.getMessage(), e);
		}
	}

	protected void getResources(final List<Object> selectionList, final List<String> resources) {
		for (Object object : selectionList) {
			if (object instanceof IContainer) {
				getResources(((IContainer) object), resources);
			} else if (object instanceof IResource) {
				IResource resource = (IResource) object;
				resources.add(resource.getFullPath().toPortableString());
			}
		}
	}

	@Override
	public boolean isEnabled() {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		IAppServerInfo currentServer = serverManager.getCurrentServer();
		IAppServerInfo serverSelection = (IAppServerInfo) getSelection();

		return super.isEnabled() && serverSelection.equals(currentServer);
	}
}
