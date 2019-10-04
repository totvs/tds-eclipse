package br.com.totvs.tds.server.manager;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.equinox.security.storage.StorageException;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IRootInfo;
import br.com.totvs.tds.server.interfaces.IServerConstants;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.launcher.LocalAppServerLauncher;
import br.com.totvs.tds.server.model.AbstractBean;
import br.com.totvs.tds.server.model.AppServerInfo;
import br.com.totvs.tds.server.model.GroupInfo;
import br.com.totvs.tds.server.model.ItemInfo;
import br.com.totvs.tds.server.model.RootInfo;

/**
 * Implementação do serviço de provimento de servidores.
 *
 * @author acandido
 */
public final class ServerManagerImpl extends AbstractBean implements IServerManager, PropertyChangeListener {

	private static final long CURRENT_SERIAL_VERSION = 2L;

	/** Servidor corrente. */
	private IAppServerInfo currentServer;

	private boolean loading;

	/** Lista de servidores registrados. */
	private IGroupInfo rootGroupInfo;

	private ByteArrayOutputStream tempSave;

	protected IAppServerInfo auxCurrentServer;

	/**
	 * Construtor.
	 */
	public ServerManagerImpl() {
		rootGroupInfo = new RootInfo();
		hookChangeListener(rootGroupInfo);
	}

	@Override
	public List<IServerInfo> getActiveServers() {
		return getActiveServers(IServerInfo.class);
	}

	private List<IServerInfo> getServers(final Class<? extends IServerInfo> clazz, final boolean connected) {
		final List<IServerInfo> list = new ArrayList<IServerInfo>();
		//
		for (final IServerInfo server : getServers(clazz)) {
			if (server.isConnected() == connected) {
				list.add(server);
			}
		}
		//
		return list;
	}

	@Override
	public List<IServerInfo> getInactiveServers(final Class<? extends IServerInfo> clazz) {
		//
		return getServers(clazz, false);
	}

	@Override
	public List<IServerInfo> getActiveServers(final Class<? extends IServerInfo> clazz) {
		//
		return getServers(clazz, true);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerManager#getCurrentServer()
	 */
	@Override
	public IAppServerInfo getCurrentServer() {
		return currentServer;
	}

	@Override
	public IGroupInfo getGroup(final String name) {
		IGroupInfo getGroup = null;

		if (name != null) {
			for (final IGroupInfo group : getGroups()) {
				final String nameGroup = group.getName().trim();
				if (name.trim().equalsIgnoreCase(nameGroup)) {
					getGroup = group;
					break;
				}
			}
		}

		return getGroup;
	}

	private List<IGroupInfo> getGroups() {
		final List<IGroupInfo> list = new ArrayList<IGroupInfo>();
		//
		for (final IItemInfo group : rootGroupInfo.toList(IGroupInfo.class)) {
			list.add((IGroupInfo) group);
		}
		//
		return list;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerService#getItems()
	 */
	@Override
	public synchronized IGroupInfo getItems() {
		return rootGroupInfo;
	}

	@Override
	public List<IServerInfo> getMonitoringServers() {
		final List<IServerInfo> list = new ArrayList<IServerInfo>();
		final List<IServerInfo> servers = getActiveServers();

		for (final IServerInfo si : servers) {
			if (si.isMonitoring()) {
				list.add(si);
			}
		}

		return list;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerService#getServer(String);
	 */
	@Override
	public IServerInfo getServer(final String name) {
		IServerInfo getServer = null;
		if (name != null) {
			for (final IServerInfo server : getServers(IServerInfo.class)) {
				if (name.equalsIgnoreCase(server.getName())) {
					getServer = server;
					break;
				}
			}
		}
		return getServer;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerService#getServer(URI);
	 */
	@Override
	public IServerInfo getServer(final URI address) {
		IServerInfo ret = null;
		//
		for (final IServerInfo server : getServers()) {
			if ((server.getAddress() != null) && server.getAddress().equals(address)) {
				ret = server;
				break;
			}
		}
		//
		return ret;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerService#getServerList()
	 */
	@Override
	public List<IServerInfo> getServers() {
		return getServers(IServerInfo.class);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerManager#getServers(Class<IServerInfo>)
	 */
	@Override
	public List<IServerInfo> getServers(final Class<? extends IServerInfo> clazzServerInfo) {
		final List<IServerInfo> list = new ArrayList<IServerInfo>();
		//
		for (final IItemInfo server : rootGroupInfo.toList(clazzServerInfo)) {
			if (server instanceof IServerInfo) {
				list.add((IServerInfo) server);
			}
		}
		//
		return list;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerService#getSize();
	 */
	@Override
	public int getSize() {
		return rootGroupInfo.getChildren().size();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see
	 * br.com.totvs.server.IServerManager#hookChangeListener(br.com.totvs.server
	 * .IGroupInfo)
	 */
	@Override
	public void hookChangeListener(final IGroupInfo items2) {
		items2.addPropertyChangeListener(this);

		for (final IItemInfo element : items2.getChildren()) {
			if (element instanceof IGroupInfo) {
				hookChangeListener((IGroupInfo) element);
			} else {
				element.addPropertyChangeListener(this);
			}
		}
	}

	private void unhookChangeListener(final IGroupInfo items2) {
		items2.removePropertyChangeListener(this);

		for (final IItemInfo element : items2.getChildren()) {
			if (element instanceof IGroupInfo) {
				unhookChangeListener((IGroupInfo) element);
			} else {
				element.removePropertyChangeListener(this);
			}
		}
	}

	@Override
	public boolean isLoading() {
		return loading;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerManager#loadFrom(java.io.InputStream)
	 */
	@Override
	public void loadFrom(final InputStream inputStream) throws ClassNotFoundException, IOException {
		setLoading(true);
		unhookChangeListener(rootGroupInfo);

		try {
			final ObjectInputStream ois = new ObjectInputStream(inputStream);
			final long serialVerion = ois.readLong();

			if (serialVerion != CURRENT_SERIAL_VERSION) {
				ServerActivator.logStatus(IStatus.WARNING, "Gerenciador Servidores",
						String.format("WARNING: ServerManagerImpl.loadFrom() CurrentVersion %d Load Version %d.",
								CURRENT_SERIAL_VERSION, serialVerion));
			}

			final IGroupInfo rootGroupAux = (IGroupInfo) ois.readObject();
			if (rootGroupAux != null) {
				final String rootName = rootGroupInfo.getName();
				rootGroupInfo.setName(rootName);

				if (rootGroupAux instanceof IRootInfo) {
					rootGroupInfo = rootGroupAux;
				} else {
					rootGroupAux.getChildren().stream().forEach(s -> rootGroupInfo.addChild(s));
				}
			}

			auxCurrentServer = (IAppServerInfo) ois.readObject();

			@SuppressWarnings("unchecked")
			final List<String> serversRunning = (ArrayList<String>) ois.readObject();
			ois.close();

			final List<IServerInfo> activeServers = getActiveServers();

			final Job job = new Job("Reconexões") {

				@Override
				protected IStatus run(final IProgressMonitor monitor) {

					if (!serversRunning.isEmpty()) {
						monitor.subTask("Iniciando servidores locais");

						for (final String name : serversRunning) {
							final IServerInfo server = getServer(name);
							if (server instanceof IAppServerInfo) {
								final IAppServerInfo appServer = (IAppServerInfo) server;
								final LocalAppServerLauncher launcher = new LocalAppServerLauncher(appServer.getName(),
										appServer.getAppServerPath());
								launcher.start();
								appServer.setLauncher(launcher);
							}
						}
					}

					if (!activeServers.isEmpty()) {
						try {
							monitor.subTask("Conexão a servidores");
							final IServiceLocator serviceLocator = PlatformUI.getWorkbench();
							final ICommandService commandService = serviceLocator.getService(ICommandService.class);
							final IHandlerService handlerService = serviceLocator.getService(IHandlerService.class);
							final Command command = commandService
									.getCommand("br.com.totvs.tds.ui.server.commands.revalidateCommand"); //$NON-NLS-1$

							for (final IServerInfo serverInfo : activeServers) {
								if (serverInfo instanceof IAppServerInfo) {
									final Map<String, Object> parameters = new HashMap<String, Object>();
									parameters.put("server", serverInfo.getName()); //$NON-NLS-1$
									parameters.put("environment", //$NON-NLS-1$
											((IAppServerInfo) serverInfo).getCurrentEnvironment());

									final ParameterizedCommand pc = ParameterizedCommand.generateCommand(command,
											parameters);
									handlerService.executeCommand(pc, null);
								}
							}
						} catch (final IllegalArgumentException e) {
							ServerActivator.logStatus(IStatus.ERROR, "Reconexão", e.getMessage(), e);
						} catch (final Exception e) {
							ServerActivator.logStatus(IStatus.ERROR, "Reconexão", e.getMessage(), e);
						}
					}

					if ((auxCurrentServer != null) && (auxCurrentServer.isConnected())) {
						ServerManagerImpl.this.setCurrentServer(auxCurrentServer);
					} else {
						ServerManagerImpl.this.setCurrentServer(null);
					}

					monitor.done();

					return Status.OK_STATUS;
				}
			};

			job.schedule();
		} finally {
			hookChangeListener(rootGroupInfo);
			setLoading(false);
			refresh();
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerService#newServer(java.lang.String,
	 * br.com.totvs.tds.server.ServerType)
	 */
	@Override
	public IAppServerInfo newAppServer(final String name) {
		final IAppServerInfo serverInfo = new AppServerInfo(name);
		//
		serverInfo.addPropertyChangeListener(this);
		//
		return serverInfo;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerManager#newGroup(java.lang.String)
	 */
	@Override
	public IGroupInfo newGroup(final String name) {
		return new GroupInfo(name);
	}

	@Override
	public void propertyChange(final PropertyChangeEvent evt) {
		if (!isLoading()) {
			firePropertyChange(evt);
		}
	}

	@Override
	public void refresh() {
		refresh(null);
	}

	@Override
	public void refresh(final IItemInfo itemInfo) {
		firePropertyChange("_refresh_", itemInfo, itemInfo); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerService#removeServer(br.com.totvs.tds.
	 * server.ServerType)
	 */
	@Override
	public void remove(final IItemInfo element) {
		rootGroupInfo.removeChild(element);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerService#removeAll();
	 */
	@Override
	public void removeAll() {
		rootGroupInfo.getChildren().clear();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerManager#saveTo(java.io.OutputStream)
	 */
	@Override
	public void saveTo(final OutputStream outputStream) throws IOException {
		final ObjectOutputStream oos = new ObjectOutputStream(outputStream);

		oos.writeLong(CURRENT_SERIAL_VERSION);

		oos.writeObject(rootGroupInfo);
		oos.writeObject(currentServer);

		final List<String> serversRunning = new ArrayList<String>();
		for (final IServerInfo server : getServers()) {
			if (server instanceof IAppServerInfo) {
				if (((IAppServerInfo) server).isRunning()) {
					serversRunning.add(server.getName());
				}
			}
		}
		oos.writeObject(serversRunning);

		oos.flush();
		oos.close();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerManager#setCurrentServer(IServerInfo)
	 */
	@Override
	public void setCurrentServer(final IAppServerInfo newServer) {
		boolean change = false;
		// verifica a necessidade da troca
		if ((currentServer == null) && (newServer != null)) {
			change = true;
		} else if ((currentServer != null) && (newServer == null)) {
			change = true;
		} else if ((currentServer != null) && !currentServer.equals(newServer)) {
			change = true;
		}
		// notifica e efetua a troca, se necessário
		if (change) {
			firePropertyChange("currentServer", this.currentServer, this.currentServer = newServer);
		}
	}

	private void setLoading(final boolean loading) {
		firePropertyChange("loading", this.loading, this.loading = loading); //$NON-NLS-1$
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerService#validade(java.lang.String,
	 * java.net.URI)
	 */
	@Override
	public void validate(final String name, final URI address) throws RuntimeException {
		// Validacao de nome
		if (!ItemInfo.isValidName(name)) {
			throw new RuntimeException("RuntimeException.SERVER_INVALIDNAME, Messages.ServerManagerImpl_3, name");
		}
		// Validacao de URI (address)
		if (address.getHost() == null) {
			throw new RuntimeException("RuntimeException.URI_INVALIDHOST, Messages.ServerManagerImpl_4, address");
		}
		if (address.getPort() < 0) {
			throw new RuntimeException("RuntimeException.URI_INVALIDPORT, Messages.ServerManagerImpl_5, address");
		}
		// Validacao de duplicidade de nome
		IServerInfo si = getServer(name);
		if (si != null) {
			throw new RuntimeException("RuntimeException.SERVER_DUPLICATEDNAME, Messages.ServerManagerImpl_6, name");
		}
		// Validacao de duplicidade de URI (address)
		si = getServer(address);
		if (si != null) {
			throw new RuntimeException("RuntimeException.SERVER_DUPLICATEDURI, Messages.ServerManagerImpl_7, address");
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Object#finalize()
	 */
	@Override
	protected void finalize() {
		rootGroupInfo.removePropertyChangeListener(this);
	}

	@Override
	protected void firePropertyChange(final String propertyName, final Object oldValue, final Object newValue) {
		if (!loading || (propertyName.equals("loading"))) {
			Display.getDefault().syncExec(() -> super.firePropertyChange(propertyName, oldValue, newValue));
		}
	}

	@Override
	protected void firePropertyChange(final PropertyChangeEvent event) {
		Display.getDefault().syncExec(() -> super.firePropertyChange(event));

		if (event.getPropertyName().equals("connected")) {
			final IAppServerInfo server = (IAppServerInfo) event.getSource();
			final Boolean isConnected = (Boolean) event.getNewValue();

			if (!isConnected && (this.currentServer != null) && (this.currentServer.equals(server))) {
				setCurrentServer(null);
			}
		}
	}

	@Override
	public void save() {
		tempSave = new ByteArrayOutputStream();
		try {
			saveTo(tempSave);
		} catch (final IOException e) {
			e.printStackTrace();
		}
	}

	@Override
	public void restore() {
		try {
			try {
				loadFrom(new ByteArrayInputStream(tempSave.toByteArray()));
			} catch (final ClassNotFoundException e) {
				e.printStackTrace();
			}
		} catch (final IOException e) {
			e.printStackTrace();
		}
	}

	private void deleteSecureStorageServerNode(final IAppServerInfo serverInfo) {
		final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
		final String node = String.format("developerStudio/%s", serverInfo.getId()); //$NON-NLS-1$

		if (securePreference.nodeExists(node)) {
			securePreference.node(node).removeNode();
			try {
				securePreference.flush();
			} catch (final IOException e) {
				ServerActivator.logStatus(IStatus.ERROR, "Armazenamento Seguro", e.getMessage(), e);
			}
		}
	}

	/**
	 * Returns the key used to store the server information in the SecureStorage
	 * area.<br>
	 * This method sets the environment name to upperCase.
	 *
	 * @param serverInfo  - The server info
	 * @param environment - The environment name. (Which will be set to upper case)
	 * @return
	 */
	public String getNodeServerKey(final IAppServerInfo serverInfo, final String environment) {
		return String.format("developerStudio/%s/%s", serverInfo.getId(), environment.toUpperCase()); //$NON-NLS-1$
	}

	private void saveLoginInfo(final IAppServerInfo serverInfo, final String environment)
			throws StorageException, IOException {
		final Map<String, Object> connectionMap = serverInfo.getConnectionMap();
		final String node = getNodeServerKey(serverInfo, environment);
		final ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
		final ISecurePreferences credencial = securePreference.node(node);

		credencial.put(IServerConstants.USERNAME, (String) connectionMap.get(IServerConstants.USERNAME), true);
		credencial.put(IServerConstants.PASSWORD, (String) connectionMap.get(IServerConstants.PASSWORD), true);

		credencial.flush();
	}

	private void updateSecureStorage(final IAppServerInfo serverInfo) throws StorageException, IOException {
		final Map<String, Object> connectionMap = serverInfo.getConnectionMap();
		final String environment = (String) connectionMap.getOrDefault(IServerConstants.ENVIRONMENT, "");
		final boolean useSecureStorage = connectionMap.getOrDefault(IServerConstants.USE_SECURE_STORAGE, false)
				.equals(true);

		if (useSecureStorage) {
			saveLoginInfo(serverInfo, environment);
		} else {
			deleteSecureStorageServerNode(serverInfo);
		}
	}

}
