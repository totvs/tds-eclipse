package br.com.totvs.tds.ui.server.handlers;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIActivator;

/**
 * Conecta ao servidor selecionado.
 *
 * @author acandido
 */
public class ConnectHandler extends ServerHandler {

	@Override
	public Object execute(final ExecutionEvent event) {
		String serverName = event.getParameter("server"); //$NON-NLS-1$
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		IAppServerInfo server = (IAppServerInfo) serverManager.getServer(serverName);

		if (server == null) {
			server = (IAppServerInfo) getSelection();
		}

		try {
			String loginDialog = server.getServerType().getLoginDialog();

			if (loginDialog != null) {
				IServiceLocator serviceLocator = PlatformUI.getWorkbench();
				ICommandService commandService = serviceLocator.getService(ICommandService.class);

				Command command = commandService.getCommand("br.com.totvs.tds.ui.server.commands.loginCommand"); //$NON-NLS-1$

				Map<String, Object> parameters = new HashMap<String, Object>();
				parameters.put("loginDialog", loginDialog); //$NON-NLS-1$
				parameters.put("server", server.getName()); //$NON-NLS-1$

				ParameterizedCommand pc = ParameterizedCommand.generateCommand(command, parameters);

				IHandlerService handlerService = serviceLocator.getService(IHandlerService.class);
				handlerService.executeCommand(pc, null);
			}

			event.getCommand().setEnabled(null);
		} catch (Exception e) {
			ServerUIActivator.logStatus(IStatus.ERROR, e.getMessage(), e);
		}
		//
		return null;
	}

	@Override
	public boolean isEnabled() {
		IServerInfo server = (IServerInfo) getSelection();
		return server != null && !server.isConnected() && super.isEnabled();
	}

}
