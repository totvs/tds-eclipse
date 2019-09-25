package br.com.totvs.tds.ui.server.internal.fileSystem;

import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryFileNode;

public class ServerDirectoryFileNode extends ServerDirectoryItemNode implements IServerDirectoryFileNode {

	public ServerDirectoryFileNode(final String name, final String absolutPath, final boolean getFiles) {
		super(name, absolutPath, getFiles);
	}

}
