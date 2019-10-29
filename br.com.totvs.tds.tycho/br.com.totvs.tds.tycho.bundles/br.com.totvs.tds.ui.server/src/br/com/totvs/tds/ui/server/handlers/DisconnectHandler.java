package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * Desconectar do servidor selecionado.
 *
 * @author acandido
 */
public class DisconnectHandler extends ServerHandler {

	@Override
	public Object execute(final ExecutionEvent event) {
		String server = event.getParameter("server"); //$NON-NLS-1$
		IAppServerInfo item = null;

		if (server != null) {
			IServerManager serverManager = ServerActivator.getDefault().getServerManager();
			item = serverManager.getServer(server);
		} else if (getSelection() instanceof IAppServerInfo) {
			item = (IAppServerInfo) getSelection();
		}

		try {
			item.disconnect();
			ServerUIActivator.logStatus(IStatus.INFO, Messages.DisconnectHandler_disconect_ok, item.getName());
		} catch (Exception e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		} finally {
			item.setConnected(false);
		}

		return null;
	}

	@Override
	public boolean isEnabled() {
		IAppServerInfo item = (IAppServerInfo) getSelection();
		return item.isConnected() && super.isEnabled();
	}

}
