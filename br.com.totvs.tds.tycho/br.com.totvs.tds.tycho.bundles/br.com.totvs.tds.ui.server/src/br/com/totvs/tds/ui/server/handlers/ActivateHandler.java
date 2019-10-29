package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIActivator;

public class ActivateHandler extends ServerHandler {

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		IItemInfo itemInfo = getSelection();

		final IAppServerInfo serverInfo = (IAppServerInfo) itemInfo.getParent();
		final String environment = itemInfo.getName();

		ServerUIActivator.logStatus(IStatus.INFO,
				"Selecionando servidor/ambiente corrente.\n\tServidor: %s\n\tAmbiente: %s", serverInfo.getName(),
				environment);

		IServerManager sm = ServerActivator.getDefault().getServerManager();
		serverInfo.setCurrentEnvironment(environment);
		sm.setCurrentServer(serverInfo);

		return null;
	}

	@Override
	public boolean isEnabled() {
		IItemInfo itemInfo = getSelection();
		return itemInfo != null && ((IAppServerInfo) itemInfo.getParent()).isConnected() && super.isEnabled();
	}
}
