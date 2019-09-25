package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.launcher.LocalAppServerLauncher;
import br.com.totvs.tds.ui.server.nl.Messages;

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
			LocalAppServerLauncher launcher = new LocalAppServerLauncher(server.getName(), server.getAppServerPath());
			launcher.start();
			server.setLauncher(launcher);
		} catch (Exception e) {
			ServerActivator.logStatus(IStatus.ERROR, Messages.ConnectHandler_connect, e.getMessage(), e);
		}
		//
		return null;
	}

	@Override
	public boolean isEnabled() {
		IAppServerInfo server = (IAppServerInfo) getSelection();
		return server != null && server.isAppServerLocal() && !server.isRunning();
	}

}
