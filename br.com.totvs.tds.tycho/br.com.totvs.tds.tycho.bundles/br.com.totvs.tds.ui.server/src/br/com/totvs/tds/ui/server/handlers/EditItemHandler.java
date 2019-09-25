package br.com.totvs.tds.ui.server.handlers;

import java.lang.reflect.Constructor;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.dialog.EditTitleAreaDialog;

/**
 * Aciona a adição de novo �tem.
 *
 * @author acandido
 */

public class EditItemHandler extends ServerHandler {

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

		// determina o diálogo a ser utilizado
		Dialog dialog = null;
		try {
			String editDialog = event.getParameter("dialog"); //$NON-NLS-1$
			Class<?> t = Class.forName(editDialog);
			Constructor<?> constructor = t.getDeclaredConstructor(Shell.class);
			dialog = (Dialog) constructor.newInstance(Display.getCurrent().getActiveShell());
		} catch (Exception e) {
			ServerActivator.logStatus(IStatus.ERROR, "Servidor", e.getMessage(), e);
			return null;
		}

		// executa o diálogo
		((EditTitleAreaDialog) dialog).setItemInfo(selectionItem);
		dialog.open();

		if (dialog.getReturnCode() == Window.OK) {
			serverManager.save();
		} else {
			serverManager.restore();
		}

		return null;
	}

}
