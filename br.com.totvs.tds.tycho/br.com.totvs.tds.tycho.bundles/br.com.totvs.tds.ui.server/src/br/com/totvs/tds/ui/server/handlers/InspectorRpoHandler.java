package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.model.RPOTypeElement;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.dialog.RpoInspectorDialog;

public class InspectorRpoHandler extends ServerHandler {

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		final RPOTypeElement rpoTypeElement = RPOTypeElement
				.valueOf(event.getParameter("br.com.totvs.tds.ui.server.rpoTypeElementParameter").toUpperCase()); //$NON-NLS-1$

		IItemInfo itemInfo = getSelection();

		final IAppServerInfo serverInfo = (IAppServerInfo) itemInfo.getParent();
		final String environment = itemInfo.getName();

		ServerUIActivator.logStatus(IStatus.INFO, "Inspetor de %s acionado.\n\tServidor: %s\n\tAmbiente: %s",
				rpoTypeElement.getTitle(), serverInfo.getName(), environment);

		Shell shell = Display.getCurrent().getActiveShell();
		RpoInspectorDialog dialog = new RpoInspectorDialog(shell, serverInfo, environment);
		dialog.setObjectType(rpoTypeElement);
		dialog.open();

		return null;
	}

}
