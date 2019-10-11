package br.com.totvs.tds.ui.server.handlers;

import java.beans.PropertyChangeListener;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.window.Window;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.ui.PlatformUI;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.wizards.INewItemWizard;

/**
 * Aciona a adição de novo �tem.
 *
 * @author acandido
 */

public class NewItemHandler extends ServerHandler {

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands.
	 * ExecutionEvent)
	 */
	@Override
	public Object execute(final ExecutionEvent event) {

		// recupera o elemento pai
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		IItemInfo selectionItem = getSelection();
		IGroupInfo element = null;

		while ((element == null) && (selectionItem != null)) {
			if (selectionItem instanceof IGroupInfo) {
				element = (IGroupInfo) selectionItem;
			} else {
				selectionItem = selectionItem.getParent();
			}
		}

		if (element == null) {
			element = serverManager.getItems();
		}

		// determina o assistente a ser utilizado
		INewItemWizard wizard = null;
		try {
			String newWizard = event.getParameter("wizard"); //$NON-NLS-1$
			Class<?> t = Class.forName(newWizard);
			wizard = (INewItemWizard) t.newInstance();
		} catch (Exception e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
			return null;
		}

		// executa o assistente
		wizard.setParentItem(element);
		WizardDialog dlg = new WizardDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(), wizard);
		dlg.open();

		if (dlg.getReturnCode() == Window.OK) {
			IItemInfo newItem = wizard.getNewItem();
			newItem.addPropertyChangeListener((PropertyChangeListener) serverManager);
			newItem.setParent(element);
		}

		return null;
	}

}
