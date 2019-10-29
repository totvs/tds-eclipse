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
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IAuthorizationKey;
import br.com.totvs.tds.server.interfaces.IGroupInfo;
import br.com.totvs.tds.server.interfaces.IItemInfo;
import br.com.totvs.tds.server.interfaces.IRootInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.server.model.AbstractBean;
import br.com.totvs.tds.server.model.AppServerInfo;
import br.com.totvs.tds.server.model.AuthorizationKey;
import br.com.totvs.tds.server.model.GroupInfo;
import br.com.totvs.tds.server.model.ItemInfo;
import br.com.totvs.tds.server.model.RootInfo;

/**
 * Implementação do serviço de provimento de servidores.
 *
 * @author acandido
 */
public final class ServerManagerImpl extends AbstractBean implements IServerManager, PropertyChangeListener {

	@SuppressWarnings("unused")
	private static final long serialVersionUID = 1L;

	private static final long CURRENT_SERIAL_VERSION = 2L;

	/** Servidor corrente. */
	private IAppServerInfo currentServer;

	private boolean loading;

	private IGroupInfo rootGroupInfo;
	private ByteArrayOutputStream tempSave;
	private IAuthorizationKey authorizationKey;

	/**
	 * Construtor.
	 */
	public ServerManagerImpl() {
		authorizationKey = new AuthorizationKey();
		rootGroupInfo = new RootInfo();
		hookChangeListener(rootGroupInfo);
	}

	@Override
	public List<IAppServerInfo> getActiveServers() {
		return getActiveServers(IAppServerInfo.class);
	}

	private List<IAppServerInfo> getServers(final Class<? extends IAppServerInfo> clazz, final boolean connected) {
		final List<IAppServerInfo> list = new ArrayList<IAppServerInfo>();
		//
		for (final IAppServerInfo server : getServers(clazz)) {
			if (server.isConnected() == connected) {
				list.add(server);
			}
		}
		//
		return list;
	}

	@Override
	public List<IAppServerInfo> getInactiveServers(final Class<? extends IAppServerInfo> clazz) {
		//
		return getServers(clazz, false);
	}

	@Override
	public List<IAppServerInfo> getActiveServers(final Class<? extends IAppServerInfo> clazz) {
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
	public List<IAppServerInfo> getMonitoringServers() {
		final List<IAppServerInfo> list = new ArrayList<IAppServerInfo>();
		final List<IAppServerInfo> servers = getActiveServers();

		for (final IAppServerInfo si : servers) {
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
	public IAppServerInfo getServer(final String name) {
		IAppServerInfo getServer = null;

		if (name != null) {
			for (final IAppServerInfo server : getServers(IAppServerInfo.class)) {
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
	public IAppServerInfo getServer(final URI address) {
		IAppServerInfo ret = null;
		//
		for (final IAppServerInfo server : getServers()) {
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
	public List<IAppServerInfo> getServers() {
		return getServers(IAppServerInfo.class);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerManager#getServers(Class<IAppServerInfo>)
	 */
	@Override
	public List<IAppServerInfo> getServers(final Class<? extends IAppServerInfo> clazzServerInfo) {
		final List<IAppServerInfo> list = new ArrayList<IAppServerInfo>();
		//
		for (final IItemInfo server : rootGroupInfo.toList(clazzServerInfo)) {
			if (server instanceof IAppServerInfo) {
				list.add((IAppServerInfo) server);
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

		if (inputStream.available() == 0) {
			ServerActivator.logStatus(IStatus.WARNING, "Arquivo de servidores vazio.");
			return;
		}

		try {
			final ObjectInputStream ois = new ObjectInputStream(inputStream);
			final long serialVerion = ois.readLong();

			if (serialVerion != CURRENT_SERIAL_VERSION) {
				ServerActivator.logStatus(IStatus.WARNING,
						String.format(Messages.ServerManagerImpl_Server_manager_version_warning, CURRENT_SERIAL_VERSION,
								serialVerion));
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

			final String currentServerName = (String) ois.readObject();

			@SuppressWarnings("unchecked")
			final List<String> serversRunning = (ArrayList<String>) ois.readObject();
			ois.close();

			final List<IAppServerInfo> activeServers = getActiveServers();

			startLocalServesAndConnections(currentServerName, serversRunning, activeServers, 3000);
		} finally {
			hookChangeListener(rootGroupInfo);
			setLoading(false);
			refresh();
		}
	}

	private void startLocalServesAndConnections(final String currentServerName, final List<String> serversRunning,
			final List<IAppServerInfo> activeServers, final int delay) {

		final Job job = new Job(Messages.ServerManagerImpl_Reconnections) {

			@Override
			protected IStatus run(final IProgressMonitor monitor) {
				final MultiStatus result = new MultiStatus(ServerActivator.PLUGIN_ID, 0, "Reconexão automática", null);

				try {
					if (!serversRunning.isEmpty()) {
						monitor.subTask(Messages.ServerManagerImpl_Startubg_local_server);

						for (final String name : serversRunning) {
							final IAppServerInfo server = getServer(name);
							server.start();
						}
					}

					if (!activeServers.isEmpty()) {
						monitor.subTask(Messages.ServerManagerImpl_Server_connect);
						ServerActivator.logStatus(IStatus.WARNING,
								Messages.ServerManagerImpl_Connecting_server_credentials_saved);

						for (final IAppServerInfo serverInfo : activeServers) {
							ServerActivator.logStatus(IStatus.INFO,
									"Reconexão automática.\n\tServidor: %s (%s)\n\tAmbiente: %s\n\tUsuário: %s",
									serverInfo.getName(), serverInfo.getAddress(), serverInfo.getCurrentEnvironment(),
									serverInfo.getUsername());
							result.add(doReconnect(serverInfo));
						}
					}

					Display.getDefault().syncExec(() -> {
						final IAppServerInfo server = getServer(currentServerName);

						if ((server != null) && (server.isConnected())) {
							ServerManagerImpl.this.setCurrentServer(server);
						} else {
							ServerManagerImpl.this.setCurrentServer(null);
						}
					});
				} catch (final Exception e) {
					result.add(ServerActivator.logStatus(IStatus.ERROR, e.getMessage(), e));
				}

				if (result.isOK()) {
					return Status.OK_STATUS;
				}

				return result;
			}
		};
		job.schedule(delay);
	}

	protected IStatus doReconnect(final IAppServerInfo serverInfo) {
		boolean isLogged = false;
		IStatus result = Status.OK_STATUS;

		try {
			final Map<String, Object> connectionMap = serverInfo.getConnectionMap();
			isLogged = serverInfo.authentication(connectionMap);

			if (!isLogged) {
				result = ServerActivator.logStatus(IStatus.ERROR, Messages.ServerManagerImpl_Connection_refused_server,
						serverInfo.getName(), serverInfo.getCurrentEnvironment());
			}
		} catch (final Exception e) {
			result = ServerActivator.logStatus(IStatus.ERROR, Messages.ServerManagerImpl_Connection_refused_server,
					serverInfo.getName(), serverInfo.getCurrentEnvironment());
			ServerActivator.logStatus(IStatus.ERROR, Messages.ServerManagerImpl_Cause, e.getMessage());
		}

		return result;
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
		oos.writeObject(currentServer.getName());

		final List<String> serversRunning = new ArrayList<String>();
		for (final IAppServerInfo server : getServers()) {
			if (server.isRunning()) {
				serversRunning.add(server.getName());
				server.stop();
			}
		}

		oos.writeObject(serversRunning);

		oos.flush();
		oos.close();
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see br.com.totvs.tds.server.IServerManager#setCurrentServer(IAppServerInfo)
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
			firePropertyChange("currentServer", this.currentServer, this.currentServer = newServer); //$NON-NLS-1$
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
		// Validação de nome
		if (!ItemInfo.isValidName(name)) {
			throw new RuntimeException("RuntimeException.SERVER_INVALIDNAME, Messages.ServerManagerImpl_3, name"); //$NON-NLS-1$
		}

		// Validação de URI (address)
		if (address.getHost() == null) {
			throw new RuntimeException("RuntimeException.URI_INVALIDHOST, Messages.ServerManagerImpl_4, address"); //$NON-NLS-1$
		}
		if (address.getPort() < 0) {
			throw new RuntimeException("RuntimeException.URI_INVALIDPORT, Messages.ServerManagerImpl_5, address"); //$NON-NLS-1$
		}

		// Validação de duplicidade de nome
		IAppServerInfo si = getServer(name);
		if (si != null) {
			throw new RuntimeException("RuntimeException.SERVER_DUPLICATEDNAME, Messages.ServerManagerImpl_6, name"); //$NON-NLS-1$
		}

		// Validação de duplicidade de URI (address)
		si = getServer(address);
		if (si != null) {
			throw new RuntimeException("RuntimeException.SERVER_DUPLICATEDURI, Messages.ServerManagerImpl_7, address"); //$NON-NLS-1$
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
	protected void firePropertyChange(final PropertyChangeEvent event) {
		if (!loading || (event.getPropertyName().equals("loading"))) { //$NON-NLS-1$
			if (event.getPropertyName().equals("connected")) { //$NON-NLS-1$
				final IAppServerInfo server = (IAppServerInfo) event.getSource();
				final Boolean isConnected = (Boolean) event.getNewValue();

				if (!isConnected && (this.currentServer != null) && (this.currentServer.equals(server))) {
					setCurrentServer(null);
				}
			}

			Display.getDefault().syncExec(() -> super.firePropertyChange(event));
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

	@Override
	public IAuthorizationKey getAuthorizationKey() {

		return authorizationKey;
	}
}
