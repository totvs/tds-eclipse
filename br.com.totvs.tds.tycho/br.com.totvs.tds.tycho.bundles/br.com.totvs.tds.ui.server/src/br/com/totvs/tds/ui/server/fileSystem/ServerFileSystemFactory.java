package br.com.totvs.tds.ui.server.fileSystem;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.ui.server.internal.fileSystem.ServerDirectoryDirNode;
import br.com.totvs.tds.ui.server.internal.fileSystem.ServerDirectoryFileNode;
import br.com.totvs.tds.ui.server.internal.fileSystem.ServerDirectoryItemNode;
import br.com.totvs.tds.ui.server.internal.fileSystem.ServerDirectoryServerNode;

/**
 * ServerFileSystemFactory.
 *
 * @author Leo Watanabe
 *
 */
public class ServerFileSystemFactory {

	/**
	 * Cria um nó de diretório
	 *
	 * @param name
	 * @param absolutPath
	 * @param getFiles
	 * @return
	 */
	static public IServerDirectoryDirNode createDirNode(final String name, final String absolutPath,
			final boolean getFiles) {
		return new ServerDirectoryDirNode(name, absolutPath, getFiles);
	}

	/**
	 * Cria um nó de arquivo.
	 *
	 * @param name
	 * @param absolutPath
	 * @param getFiles
	 * @return
	 */
	static public IServerDirectoryFileNode createFileNode(final String name, final String absolutPath,
			final boolean getFiles) {
		return new ServerDirectoryFileNode(name, absolutPath, getFiles);
	}

	/**
	 * Cria um nó raiz.
	 *
	 * @return
	 */
	static public IServerDirectoryItemNode createRootNode() {
		return new ServerDirectoryItemNode("ROOT_NODE", "", false); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Cria um nó de servidor.
	 *
	 * @param server
	 * @param environment
	 * @param getFiles
	 * @return
	 */
	static public IServerDirectoryServerNode createServerNode(final IAppServerInfo server, final String environment,
			final boolean getFiles) {

		return new ServerDirectoryServerNode(server, environment, getFiles);
	}

}
