package br.com.totvs.tds.ui.server.vo;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;

/**
 * informações utilizadas pelos assistentes.
 *
 * @author acandido
 *
 */
public class NewServerVO {

	private IGroupInfo parent;
	private IAppServerInfo server;
	private boolean immediateConnection = true;

	public IGroupInfo getParent() {
		return parent;
	}

	public IAppServerInfo getServer() {
		return server;
	}

	public boolean isImmediateConnection() {
		return this.immediateConnection;
	}

	public void setImmediateConnection(boolean immediateConnection) {
		this.immediateConnection = immediateConnection;

	}

	public void setParent(IGroupInfo parent) {
		this.parent = parent;
	}

	public void setServer(IAppServerInfo server) {
		this.server = server;
	}

}
