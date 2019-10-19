package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.jobs.DefragRPOJob;
import br.com.totvs.tds.ui.server.ServerUIActivator;

public class MaintenanceRpoHandler extends ServerHandler {

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		final String operation = event.getParameter("br.com.totvs.tds.ui.server.rpoOperationtParameter"); //$NON-NLS-1$

		IItemInfo itemInfo = getSelection();

		final IAppServerInfo serverInfo = (IAppServerInfo) itemInfo.getParent();
		final String environment = itemInfo.getName();

		if ("defrag".equals(operation)) {
			if (MessageDialog.openQuestion(Display.getCurrent().getActiveShell(), "RPO: Desfragmentação",
					"O processo de desfragmentação pode demorar e impedir o uso do ambiente.\nConfirme para continuar.")) {
				ServerUIActivator.logStatus(IStatus.INFO,
						"Manutenção do RPO acionado.\n\tServidor: %s\n\tAmbiente: %s\n\tOperação: %s",
						serverInfo.getName(), environment, "desfragmentação");

				DefragRPOJob job = new DefragRPOJob(serverInfo, new String[] { environment });
				job.schedule();
			}

		}

		return null;
	}

}
