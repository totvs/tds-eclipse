package br.com.totvs.tds.ui.server.vo;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.ServerType;
import br.com.totvs.tds.server.model.AppServerInfo;

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
	private ServerType serverType;

	public NewServerVO() {
		server = new AppServerInfo("ForDesignOnly");
		server.setServerType(ServerType.PROTHEUS);
	}

	public IGroupInfo getParent() {
		return parent;
	}

	public IAppServerInfo getServer() {
		return server;
	}

	/**
	 * @return the serverType
	 */
	public ServerType getServerType() {
		return serverType;
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

	/**
	 * @param serverType the serverType to set
	 */
	public void setServerType(ServerType serverType) {
		this.serverType = serverType;
	}

}
