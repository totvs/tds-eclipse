package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
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
			item = (IAppServerInfo) serverManager.getServer(server);
		} else if (getSelection() instanceof IAppServerInfo) {
			item = (IAppServerInfo) getSelection();
		}

		try {
			IServiceLocator serviceLocator = PlatformUI.getWorkbench();
			ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

			lsService.disconnect(item.getName(), item.getToken());
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
		IServerInfo item = (IServerInfo) getSelection();
		return item.isConnected() && super.isEnabled();
	}

}
