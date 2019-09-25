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
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * Desconectar do servidor selecionado.
 *
 * @author acandido
 */
public class DisconnectHandler extends ServerHandler {

	/**
	 * Desconexão do servidor.
	 *
	 * @throws Exception
	 */
	private void disconnect(final IAppServerInfo connector) throws Exception {
		IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

		if (!lsService.disconnect(connector.getName(), connector.getToken())) {
			throw new Exception(Messages.DisconnectHandler_disconnect_forced);
		}
	}

	@Override
	public Object execute(final ExecutionEvent event) {
		String server = event.getParameter("server"); //$NON-NLS-1$
		IAppServerInfo item = null;

		if (server != null) {
			IServerManager serverManager = ServerActivator.getDefault().getServerManager();
			item = (IAppServerInfo) serverManager.getServer(server);
		}

		if ((item == null) && (getSelection() instanceof IAppServerInfo)) {
			item = (IAppServerInfo) getSelection();
		}

		try {
			disconnect(item);
			ServerActivator.logStatus(IStatus.INFO, Messages.DisconnectHandler_discconect,
					Messages.DisconnectHandler_disconect_ok, item.getName());
		} catch (Exception e) {
			ServerActivator.logStatus(IStatus.ERROR, Messages.DisconnectHandler_discconect, e.getMessage(), e);
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
