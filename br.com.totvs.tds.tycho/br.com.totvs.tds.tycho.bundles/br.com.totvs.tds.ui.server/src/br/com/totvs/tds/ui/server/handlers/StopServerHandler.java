package br.com.totvs.tds.ui.server.handlers;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.runtime.IStatus;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.launcher.LocalAppServerLauncher;

/**
 * Conecta ao servidor selecionado.
 *
 * @author acandido
 */
public class StopServerHandler extends ServerHandler {

	@Override
	public Object execute(final ExecutionEvent event) {
		IAppServerInfo server = (IAppServerInfo) getSelection();

		try {
			LocalAppServerLauncher launcher = (LocalAppServerLauncher) server.getLauncher();
			launcher.stop();
		} catch (Exception e) {
			ServerActivator.logStatus(IStatus.ERROR, "Servidor Local", e.getMessage(), e);
		}

		server.setLauncher(null);
		//
		return null;
	}

	@Override
	public boolean isEnabled() {
		IAppServerInfo server = (IAppServerInfo) getSelection();
		return server != null && server.isAppServerLocal() && server.isRunning();
	}

}