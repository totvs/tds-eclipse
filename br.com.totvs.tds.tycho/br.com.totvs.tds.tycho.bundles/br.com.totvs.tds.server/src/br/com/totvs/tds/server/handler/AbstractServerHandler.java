package br.com.totvs.tds.server.handler;

import org.eclipse.core.commands.AbstractHandler;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;

/**
 * Classe b�sica para execução de comandos associados a uma aplicação servidora.
 * 
 * @author acandido
 */
public abstract class AbstractServerHandler extends AbstractHandler {

	private IAppServerInfo server;

	/**
	 * @return the server
	 */
	public IAppServerInfo getServer() {
		return server;
	}

	/**
	 * @param server the server to set
	 */
	public void setServer(IAppServerInfo server) {
		this.server = server;
	}


}
