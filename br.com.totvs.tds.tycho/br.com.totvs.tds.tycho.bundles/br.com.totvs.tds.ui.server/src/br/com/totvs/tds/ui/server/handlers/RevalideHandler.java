package br.com.totvs.tds.ui.server.handlers;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.equinox.security.storage.ISecurePreferences;
import org.eclipse.equinox.security.storage.SecurePreferencesFactory;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo.ServerType;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.tools.ILoginDialog;

/**
 * Solicita os dados de conexão e realiza a conexão.
 *
 * @author acandido
 */
//TODO: separar o login com tela do login sem tela (revalidate)
public class RevalideHandler extends ServerHandler {

	/**
	 * Returns the key used to store the server information in the SecureStorage
	 * area.<br>
	 * This must be used when the server was connected but there was no environment
	 * set.
	 *
	 * @param serverInfo - The server info
	 * @return
	 */
	public static String getNodeServerKey(final IAppServerInfo serverInfo) {
		return getNodeServerKey(serverInfo, "<empty>"); //$NON-NLS-1$
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
	public static String getNodeServerKey(final IAppServerInfo serverInfo, final String environment) {
		if (environment == null) {
			return getNodeServerKey(serverInfo);
		}

		return String.format("developerStudio/%s/%s", serverInfo.getId(), environment.toUpperCase()); //$NON-NLS-1$
	}

	private String connectMessage = Messages.EMPTY_STRING;
	private String dialog;
	private String environment;
	private Boolean forceUser;

	private Boolean revalidate;
	private IAppServerInfo server;

	private String serverName;

//	private boolean loginLogix(final String[] environmentsAllowed, final IAppServerInfo connector,
//			final String allEnvironments) throws Exception {
//		boolean isLoggedIn = false;
//		Map<String, Object> inputMap = new HashMap<String, Object>();
//		Map<String, String> connectionMap = new HashMap<String, String>();
//		boolean isConnected = connectLogix(connector);
//		if (isConnected) {
//			return true;
//		}
//		inputMap = this.getEnvironmentsMap(environmentsAllowed, this.environment, server, allEnvironments);
//		ILoginDialog dialogLogin = this.getLoginDialog(dialog, server, inputMap);
//		int open = dialogLogin.open();
//		if (open == Dialog.OK) {
//			isLoggedIn = doLoginLogix(connector, open);
//		}
//
//		return isLoggedIn;
//	}

//	private boolean connectLogix(final IAppServerInfo connector) {
//		String environment = (this.environment == null) ? server.getCurrentEnvironment() : this.environment;
//
//		if (environment == null) {
//			return false;
//		}
//
//		connector.getConnectionMap().put("environment", environment); //$NON-NLS-1$
//		doLogin(connector, false);
//
//		return connector.isConnected();
//	}

//	private boolean doLoginLogix(final IAppServerInfo connector,
//			final int fOpen) {
//		int open = fOpen;
//		connector.getConnectionMap().clear();
//
//		if (open == Dialog.OK) {
//			doLogin(connector, false);
//			return true;
//		} else {
//			logger.error("Informações estão incorretas");
//		}
//
//		return false;
//	}

	Runnable loginRunnable = new Runnable() {
		@Override
		public void run() {
			try {
				ServerType serverType = server.getServerType();
				boolean ok = false;

				if (ServerType.PROTHEUS.equals(serverType)) { // $NON-NLS-1$
					if (revalidate) {
						ok = loginProtheus(server, false);
					} else {
						ok = loginProtheus(server, forceUser);
					}
				} else if (ServerType.LOGIX.equals(serverType)) { // $NON-NLS-1$
					ok = false; // loginLogix(environmentsAllowed, server, allEnvironments);
				} else {
					ServerUIActivator.logStatus(IStatus.ERROR, Messages.LoginHandler_identification,
							Messages.LoginHandler_service_not_supported, serverType);
				}

				IServerManager serverManager = ServerActivator.getDefault().getServerManager();
				if (ok) {
					serverManager.setCurrentServer(server);
					ServerUIActivator.logStatus(IStatus.OK, Messages.LoginHandler_identification,
							Messages.LoginHandler_connection_ok, server.getName(), server.getCurrentEnvironment(),
							server.getConnectionMap().get(ILoginDialog.USERNAME));
				} else {
					serverManager.setCurrentServer(null);
					deleteSecureStorageServerNode(server);
				}
			} catch (Exception e) {
				e.printStackTrace();
				ServerUIActivator.logStatus(IStatus.ERROR, Messages.LoginHandler_identification,
						Messages.LoginHandler_connection_error);
				deleteSecureStorageServerNode(server);
			}
		}
	};

	private boolean connectWithSecureStorageCredentials(final ISecurePreferences securePreference,
			final String nodeServer, final String environment, final IAppServerInfo connector)
			throws org.eclipse.equinox.security.storage.StorageException {
		ISecurePreferences credencial = securePreference.node(nodeServer);
		String user = credencial.get(ILoginDialog.USERNAME, ""); //$NON-NLS-1$ //$NON-NLS-2$
		String password = credencial.get(ILoginDialog.PASSWORD, ""); //$NON-NLS-1$ //$NON-NLS-2$

		// se existir as informacoes de login tenta logar sem exibir a tela de login
		Map<String, Object> map = initializeConnectionMap(connector.getAddress(), environment, user, password, true);

		return doLogin(connector, map);

	}

	private void deleteSecureStorageServerNode(final IAppServerInfo serverInfo) {
		ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
		String node = getNodeServerKey(serverInfo);

		if (securePreference.nodeExists(node)) {
			securePreference.node(node).removeNode();
			try {
				securePreference.flush();
			} catch (IOException e) {
				ServerUIActivator.logStatus(IStatus.ERROR, Messages.LoginHandler_identification, e.getMessage(), e);
			}
		}
	}

	/**
	 * Realiza a conexão do servidor.
	 *
	 * @param serverManager - Gerenciador de servidores
	 * @param connectionMap - Dados de conexão do servidor
	 * @param connector     - Conector do servidor
	 */
	private boolean doLogin(final IAppServerInfo connector, Map<String, Object> connectionMap) {
		if (isUseSecureStorage(connectionMap)) {
			ServerUIActivator.logStatus(IStatus.INFO, Messages.LoginHandler_identification,
					Messages.LoginHandler_connection_use_credentials_secure_local);
		} else {
			ServerUIActivator.logStatus(IStatus.INFO, Messages.LoginHandler_identification,
					Messages.LoginHandler_connection_in_processs);
		}

		boolean isLogged = false;

		try {
			IServiceLocator serviceLocator = PlatformUI.getWorkbench();
			ILanguageServerService lsService = serviceLocator.getService(ILanguageServerService.class);

			isLogged = connector.authentication(lsService, connectionMap);
			if (!isLogged) {
				connectMessage = Messages.LoginHandler_connection_error_1;
				ServerUIActivator.logStatus(IStatus.ERROR, Messages.LoginHandler_identification, connectMessage);
			}
		} catch (IllegalArgumentException e) {
			ServerUIActivator.logStatus(IStatus.ERROR, Messages.LoginHandler_identification, e.getMessage(), e);
		} catch (Exception e) {
			connectMessage = Messages.LoginHandler_connection_error_1;
			ServerUIActivator.logStatus(IStatus.ERROR, Messages.LoginHandler_identification, connectMessage);
			ServerUIActivator.logStatus(IStatus.ERROR, Messages.LoginHandler_identification, e.getMessage());
			ServerUIActivator.logStatus(IStatus.WARNING, Messages.LoginHandler_identification,
					Messages.LoginHandler_connection_error_3);
		}

		return isLogged;
	}

	@Override
	public Object execute(final ExecutionEvent event) throws ExecutionException {
		dialog = event.getParameter("loginDialog"); //$NON-NLS-1$
		serverName = event.getParameter("server"); //$NON-NLS-1$
		environment = event.getParameter("environment"); //$NON-NLS-1$
		forceUser = Boolean.valueOf(event.getParameter("forceLogin")); //$NON-NLS-1$
		revalidate = Boolean.valueOf(event.getParameter("revalidate")); //$NON-NLS-1$
		server = validServer(serverName);

		Job job = new Job("Conexão") {

			@Override
			protected IStatus run(IProgressMonitor monitor) {
				Display.getDefault().asyncExec(loginRunnable);
				return server.isConnected() ? Status.OK_STATUS
						: ServerUIActivator.logStatus(IStatus.ERROR, "Conexão",
								"Não foi possível estabelecer conexão.\n\tServidor: %s", server.getName());
			}
		};
		job.schedule();

		return false;
	}

	private String getEnvironment(final Map<String, Object> connectionMap) {
		return (String) connectionMap.get(ILoginDialog.ENVIRONMENT);
	}

	/*
	 * Adiciona ambientes no map
	 */
	private Map<String, Object> getEnvironmentsMap(String environmentParameter, final IAppServerInfo server) {
		Map<String, Object> environmentMap = new HashMap<String, Object>();

		if (environmentParameter == null) {
			environmentParameter = server.getCurrentEnvironment();
		}

		List<IEnvironmentInfo> environments = server.getEnvironments();
		List<String> collect = environments.stream().map(e -> e.getName()).collect(Collectors.toList());
		String[] lastEnvironments = collect.toArray(new String[collect.size()]);

		environmentMap.put(ILoginDialog.ENVIRONMENT,
				(environmentParameter == null) ? Messages.EMPTY_STRING : environmentParameter);
		environmentMap.put(ILoginDialog.LAST_ENVIRONMENTS, lastEnvironments);

		return environmentMap;
	}

	/*
	 * Obt�m o dialog para conectar servidores e ambientes.
	 */
	private ILoginDialog getLoginDialog(final String dialogParameter, final IAppServerInfo server,
			final Map<String, Object> inputMap) throws ClassNotFoundException, NoSuchMethodException,
			InstantiationException, IllegalAccessException, InvocationTargetException {
		String dialogTitle = String.format(Messages.LoginHandler_connection_server_identify, server.getName(),
				server.getServerType());

		Class<?> clazz = Class.forName(dialogParameter);
		Constructor<?> constructor = clazz.getConstructor(new Class[] { Shell.class });
		ILoginDialog dialogLogin = (ILoginDialog) constructor
				.newInstance(PlatformUI.getWorkbench().getDisplay().getActiveShell());

		dialogLogin.initialize(dialogTitle, inputMap);

		return dialogLogin;
	}

	private Map<String, Object> initializeConnectionMap(URI uri, final String environment, final String user,
			final String password, boolean secureStorage) {
		Map<String, Object> connectionMap = new HashMap<String, Object>();

		connectionMap.put(ILoginDialog.ENVIRONMENT, environment);
		connectionMap.put(ILoginDialog.USERNAME, user);
		connectionMap.put(ILoginDialog.PASSWORD, password);
		connectionMap.put(ILoginDialog.SERVER_ADDRESS, uri);
		connectionMap.put(ILoginDialog.USE_SECURE_STORAGE, secureStorage);

		return connectionMap;
	}

	private boolean isUseSecureStorage(final Map<String, Object> connectionMap) {
		return connectionMap.getOrDefault(ILoginDialog.USE_SECURE_STORAGE, false).equals(true); // $NON-NLS-1$
	}

	private boolean loginProtheus(final IAppServerInfo connector, final boolean forceUser) throws Exception {
		String environment = (this.environment == null) ? server.getCurrentEnvironment() : this.environment;
		String nodeServer = getNodeServerKey(connector, environment);

		boolean isLoggedIn = false;
		ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();

		if (!forceUser && (securePreference != null) && securePreference.nodeExists(nodeServer)) {
			isLoggedIn = connectWithSecureStorageCredentials(securePreference, nodeServer, environment, connector);
			if (!isLoggedIn) {
				connectMessage = Messages.LoginHandler_connection_error_4;
			}
		} else {
			isLoggedIn = loginWithProtheusDialog(connector);
		}

		return isLoggedIn;
	}

	/**
	 * Opens a dialog to get the login information from the user.
	 *
	 * @param environmentsAllowed
	 * @param connector
	 * @param allEnvironments
	 * @param config
	 * @return
	 * @throws Exception The possible exceptions to be thrown by this method are:
	 *                   <br>
	 *                   ClassNotFoundException, NoSuchMethodException,
	 *                   InstantiationException, IllegalAccessException,
	 *                   InvocationTargetException, StorageException, IOException
	 */
	private boolean loginWithProtheusDialog(IAppServerInfo connector) throws Exception {
		boolean isLoggedIn = false;
		connectMessage = Messages.EMPTY_STRING; // indica qual a ocorrência no caso da conexão falhar.

		// O loop abaixo serve para que a tela fique aberta até que o usuario
		// consiga identificar-se ou cancele a solicitação
		while (!isLoggedIn) {
			ILoginDialog loginDialog = this.getLoginDialog(dialog, server,
					getEnvironmentsMap(this.environment, server));
			loginDialog.setInitialMessage(connectMessage);
			loginDialog.getDataMap().put(ILoginDialog.SERVER_ADDRESS, server.getAddress().toString());

			if (loginDialog.open() == Dialog.OK) {
				Map<String, Object> connectionMap = loginDialog.getDataMap();
				if (connectionMap == null) {
					connectMessage = Messages.LoginHandler_28;
				} else {
					isLoggedIn = doLogin(connector, connectionMap);
					updateSecureStorage(connectionMap, connector);
				}
			} else {
				break;
			}
		}

		return isLoggedIn;
	}

	private void saveLoginInfo(final IAppServerInfo serverInfo, String environment, Map<String, Object> connectionMap)
			throws IOException, org.eclipse.equinox.security.storage.StorageException {
		ISecurePreferences securePreference = SecurePreferencesFactory.getDefault();
		String node = getNodeServerKey(serverInfo, environment);

		ISecurePreferences credencial = securePreference.node(node);
		credencial.put(ILoginDialog.USERNAME, (String) connectionMap.get(ILoginDialog.USERNAME), false);
		credencial.put(ILoginDialog.PASSWORD, (String) connectionMap.get(ILoginDialog.PASSWORD), true);

		credencial.flush();
	}

	private void updateSecureStorage(final Map<String, Object> connectionMap, final IAppServerInfo serverInfo)
			throws IOException, org.eclipse.equinox.security.storage.StorageException {
		String environment = getEnvironment(connectionMap);

		if (isUseSecureStorage(connectionMap)) {
			saveLoginInfo(serverInfo, environment, connectionMap);
		} else {
			deleteSecureStorageServerNode(serverInfo);
		}
	}

	/*
	 * Valida se o servidor foi localizado.
	 */
	private IAppServerInfo validServer(final String serverName) throws ExecutionException {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		IAppServerInfo server = (IAppServerInfo) serverManager.getServer(serverName);

		if (server == null) {
			throw new ExecutionException(String.format(Messages.LoginHandler_29, serverName));
		}

		return server;
	}
}
