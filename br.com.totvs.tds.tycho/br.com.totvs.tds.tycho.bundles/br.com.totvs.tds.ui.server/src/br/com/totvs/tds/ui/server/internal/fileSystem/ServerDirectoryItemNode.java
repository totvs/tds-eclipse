package br.com.totvs.tds.ui.server.internal.fileSystem;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryFileNode;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryItemNode;
import br.com.totvs.tds.ui.server.fileSystem.IServerDirectoryServerNode;
import br.com.totvs.tds.ui.server.fileSystem.ServerFileSystemFactory;

/**
 * ServerDirectoryItemNode.
 *
 * @author Leo Watanabe
 *
 */
public class ServerDirectoryItemNode implements IServerDirectoryItemNode {

	private String name = ""; //$NON-NLS-1$
	private String absolutPath = ""; //$NON-NLS-1$
	private boolean getFiles;

	private IServerDirectoryItemNode parent;
	private List<IServerDirectoryItemNode> children;

	private boolean loaded = false;

	public ServerDirectoryItemNode(final String name, final String absolutPath, final boolean getFiles) {
		super();
		this.name = name;
		this.absolutPath = absolutPath;
		this.getFiles = getFiles;
		children = new ArrayList<IServerDirectoryItemNode>();
	}

	@Override
	public void addItemNode(final IServerDirectoryItemNode itemNode) {
		if (!children.contains(itemNode)) {
			children.add(itemNode);
			itemNode.setParent(this);
		}
	}

	@Override
	public String getAbsolutPath() {
		return absolutPath;
	}

	@Override
	public List<IServerDirectoryItemNode> getChildren() {
		if (!loaded) {
			loadChildren();
		}
		return children;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public IServerDirectoryItemNode getParent() {
		return parent;
	}

	@Override
	public IServerDirectoryServerNode getServerNode() {
		if (this instanceof IServerDirectoryServerNode) {
			return (IServerDirectoryServerNode) this;
		} else if (parent != null) {
			return parent.getServerNode();
		}
		return null;
	}

	@Override
	public boolean hasChildren() {
		if (!loaded && !(this instanceof IServerDirectoryFileNode)) {
			return true;
		}
		return children.size() > 0;
	}

	// Carrega o conte�do do n� corrente
	private void loadChildren() {
		IServerDirectoryServerNode serverNode = getServerNode();
		if (serverNode == null) {
			return;
		}
		IAppServerInfo server = serverNode.getServer();
		String environment = serverNode.getEnvironment();
		if (server != null && environment != null) {
			loaded = loadDirs(server, environment) && loadFiles(server, environment);
		}
	}

	// Carrega os diretorios do nó corrente
	private boolean loadDirs(final IAppServerInfo server, final String environment) {
		boolean loadDirs = true;
		//
		try {
			List<String> dirs = null; // server.getDirectory(environment, absolutPath, true);
			for (String dir : dirs) {
				addItemNode(ServerFileSystemFactory.getInstance().createDirNode(dir,
						absolutPath + dir + File.separatorChar, getFiles));
			}
		} catch (Exception e) {
			loadDirs = false;
		}
		//
		return loadDirs;
	}

	// Carrega os arquivos, se a flag getFiles estiver ativa, do n� corrente
	private boolean loadFiles(final IAppServerInfo server, final String environment) {
		boolean loadFiles = true;
		//
		if (getFiles) {
			try {
				List<String> files = null; // server.getDirectory(environment, absolutPath, false);
				for (String file : files) {
					addItemNode(
							ServerFileSystemFactory.getInstance().createFileNode(file, absolutPath + file, getFiles));
				}
			} catch (Exception e) {
				loadFiles = false;
			}
		}
		//
		return loadFiles;
	}

	@Override
	public void removeItemNode(final IServerDirectoryItemNode itemNode) {
		if (children.contains(itemNode)) {
			children.remove(itemNode);
			itemNode.setParent(null);
		}
	}

	@Override
	public void setAbsolutPath(final String absolutPath) {
		this.absolutPath = absolutPath;
	}

	@Override
	public void setName(final String name) {
		this.name = name;
	}

	@Override
	public void setParent(final IServerDirectoryItemNode parent) {
		this.parent = parent;
	}

}
