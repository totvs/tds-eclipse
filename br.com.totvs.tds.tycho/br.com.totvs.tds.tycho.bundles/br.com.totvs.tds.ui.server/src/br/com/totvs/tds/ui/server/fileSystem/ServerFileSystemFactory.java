package br.com.totvs.tds.ui.server.fileSystem;

import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
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

	private static ServerFileSystemFactory instance;

	/**
	 * Obtem instancia do ServerFileSystemFactory.
	 *
	 * @return
	 */
	public static ServerFileSystemFactory getInstance() {
		if (instance == null) {
			instance = new ServerFileSystemFactory();
		}

		return instance;
	}

	private ServerFileSystemFactory() {
	}

	/**
	 * Cria um n� de diret�rio.
	 *
	 * @param name
	 * @param absolutPath
	 * @param getFiles
	 * @return
	 */
	public IServerDirectoryDirNode createDirNode(final String name, final String absolutPath, final boolean getFiles) {
		return new ServerDirectoryDirNode(name, absolutPath, getFiles);
	}

	/**
	 * Cria um n� de arquivo.
	 *
	 * @param name
	 * @param absolutPath
	 * @param getFiles
	 * @return
	 */
	public IServerDirectoryFileNode createFileNode(final String name, final String absolutPath,
			final boolean getFiles) {
		return new ServerDirectoryFileNode(name, absolutPath, getFiles);
	}

	/**
	 * Cria um n� raiz.
	 *
	 * @return
	 */
	public IServerDirectoryItemNode createRootNode() {
		return new ServerDirectoryItemNode("ROOT_NODE", "", false); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/**
	 * Cria um n� de servidor.
	 *
	 * @param server
	 * @param environment
	 * @param getFiles
	 * @return
	 */
	public IServerDirectoryServerNode createServerNode(final IAppServerInfo server, final String environment,
			final boolean getFiles) {
		if (getFiles) {
			IServiceLocator serviceLocator = PlatformUI.getWorkbench();
			ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

			lsService.getPathDirList(server.getToken(), environment, "/", true);

		}

		return new ServerDirectoryServerNode(server, environment, getFiles);
	}

}
