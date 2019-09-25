package br.com.totvs.tds.ui.server.internal.fileSystem;

import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryDirNode;

public class ServerDirectoryDirNode extends ServerDirectoryItemNode implements IServerDirectoryDirNode {

	public ServerDirectoryDirNode(final String name, final String absolutPath, final boolean getFiles) {
		super(name, absolutPath, getFiles);
	}

}
