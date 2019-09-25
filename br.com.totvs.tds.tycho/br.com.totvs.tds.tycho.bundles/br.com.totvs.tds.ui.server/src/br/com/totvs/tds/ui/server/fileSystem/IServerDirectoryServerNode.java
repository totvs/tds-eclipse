package br.com.totvs.tds.ui.server.fileSystem;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;

public interface IServerDirectoryServerNode extends IServerDirectoryItemNode {

	String getEnvironment();

	IAppServerInfo getServer();

}
