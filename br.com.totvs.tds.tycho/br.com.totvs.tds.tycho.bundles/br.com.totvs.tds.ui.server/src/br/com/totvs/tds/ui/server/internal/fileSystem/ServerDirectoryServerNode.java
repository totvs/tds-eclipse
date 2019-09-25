package br.com.totvs.tds.ui.server.internal.fileSystem;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryServerNode;

public class ServerDirectoryServerNode extends ServerDirectoryItemNode implements IServerDirectoryServerNode {

	private IAppServerInfo server;
	private String environment = ""; //$NON-NLS-1$

	public ServerDirectoryServerNode(final IAppServerInfo server, final String environment, final boolean getFiles) {
		super(server.getName(), "", getFiles); //$NON-NLS-1$
		this.server = server;
		this.environment = environment;

	}

	@Override
	public String getEnvironment() {
		return environment;
	}

	@Override
	public IAppServerInfo getServer() {
		return server;
	}

}
