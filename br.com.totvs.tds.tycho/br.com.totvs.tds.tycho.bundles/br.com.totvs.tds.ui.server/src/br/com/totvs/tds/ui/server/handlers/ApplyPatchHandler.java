package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.handlers.HandlerUtil;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.jobs.applyPatch.ApplyPatchAttributes;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.wizards.patch.ApplyPatchWizard;

/**
 * Handler para aplicação de patch.
 */
public class ApplyPatchHandler extends ServerHandler {

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		IItemInfo item = getSelection();

		try {
			IAppServerInfo appServer = null;

			if (item instanceof IAppServerInfo) {
				appServer = (IAppServerInfo) item;
			} else {
				appServer = (IAppServerInfo) item.getParent();
			}
			String environment = appServer.getCurrentEnvironment();

			ApplyPatchAttributes attributes = new ApplyPatchAttributes();
			attributes.setCurrentAppServer(appServer);
			attributes.setEnvironment(environment);

			ApplyPatchWizard applyWizard = new ApplyPatchWizard(attributes);

			Shell shell = HandlerUtil.getActiveWorkbenchWindow(event).getShell();
			WizardDialog dialog = new WizardDialog(shell, applyWizard);
			dialog.setBlockOnOpen(true);
			dialog.open();
		} catch (Exception e) {
			ServerUIActivator.showStatus(IStatus.ERROR, e.getMessage(), e);
		}
		//
		return null;
	}

//	protected void getResources(final IContainer container, final List<String> resources) {
//		try {
//			for (IResource resource : container.members()) {
//				if (resource instanceof IContainer) {
//					getResources(((IContainer) resource), resources);
//				} else {
//					resources.add(resource.getFullPath().toPortableString());
//				}
//			}
//		} catch (CoreException e) {
//			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
//		}
//	}
//
//	protected void getResources(final List<Object> selectionList, final List<String> resources) {
//		for (Object object : selectionList) {
//			if (object instanceof IContainer) {
//				getResources(((IContainer) object), resources);
//			} else if (object instanceof IResource) {
//				IResource resource = (IResource) object;
//				resources.add(resource.getFullPath().toPortableString());
//			}
//		}
//	}

//	@Override
//	public boolean isEnabled() {
//		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
//		IAppServerInfo currentServer = serverManager.getCurrentServer();
//		IAppServerInfo serverSelection = (IAppServerInfo) getSelection();
//
//		return super.isEnabled() && serverSelection.equals(currentServer);
//	}
}
