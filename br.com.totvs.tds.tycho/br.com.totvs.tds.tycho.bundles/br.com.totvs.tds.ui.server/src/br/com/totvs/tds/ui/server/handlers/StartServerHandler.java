package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.ui.server.ServerUIActivator;

/**
 * Conecta ao servidor selecionado.
 *
 * @author acandido
 */
public class StartServerHandler extends ServerHandler {

	@Override
	public Object execute(final ExecutionEvent event) {
		try {
			IAppServerInfo server = (IAppServerInfo) getSelection();
			if (server.isLocalServer()) {
				server.start();
			}
		} catch (Exception e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}
		//
		return null;
	}

	@Override
	public boolean isEnabled() {
		IAppServerInfo server = (IAppServerInfo) getSelection();
		return server != null && server.isLocalServer() && !server.isRunning();
	}

}
