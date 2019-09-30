package br.com.totvs.tds.ui.server.handlers;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.lsp.server.ILanguageServerService;
import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IEnvironmentInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.nl.Messages;
import br.com.totvs.tds.ui.server.tools.ILoginDialog;

/**
 * Solicita os dados de conexão e realiza a conexão.
 *
 * @author acandido
 */
public class LoginHandler extends ServerHandler {

	private String connectMessage = Messages.EMPTY_STRING;
	private String dialog;
	private String environment;

	private IAppServerInfo server;

	private String serverName;

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

		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		server = (IAppServerInfo) serverManager.getServer(serverName);

		if (server == null) {
			throw new ExecutionException(String.format(Messages.LoginHandler_29, serverName));
		}

		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				try {
					boolean ok = openLoginDialog();

					IServerManager serverManager = ServerActivator.getDefault().getServerManager();
					if (ok) {
						serverManager.setCurrentServer(server);
						ServerUIActivator.logStatus(IStatus.OK, Messages.LoginHandler_identification,
								Messages.LoginHandler_connection_ok, server.getName(), server.getCurrentEnvironment(),
								server.getConnectionMap().get(ILoginDialog.USERNAME));
					} else {
						serverManager.setCurrentServer(null);
					}
				} catch (Exception e) {
					e.printStackTrace();
					ServerUIActivator.logStatus(IStatus.ERROR, Messages.LoginHandler_identification,
							Messages.LoginHandler_connection_error);
				}
			}
		});
		event.getCommand().setEnabled(null);

		return false;
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
		String dialogTitle = String.format(Messages.LoginHandler_connection_server_identify,
				server.getServerType().getTitle(), server.getName());

		Class<?> clazz = Class.forName(dialogParameter);
		Constructor<?> constructor = clazz.getConstructor(new Class[] { Shell.class });
		ILoginDialog dialogLogin = (ILoginDialog) constructor
				.newInstance(PlatformUI.getWorkbench().getDisplay().getActiveShell());

		dialogLogin.initialize(dialogTitle, inputMap);

		return dialogLogin;
	}

	private boolean isUseSecureStorage(final Map<String, Object> connectionMap) {
		return connectionMap.getOrDefault(ILoginDialog.USE_SECURE_STORAGE, false).equals(true); // $NON-NLS-1$
	}

	/**
	 * Opens a dialog to get the login information from the user.
	 */
	private boolean openLoginDialog() throws Exception {
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
					isLoggedIn = doLogin(server, connectionMap);
				}
			} else {
				break;
			}
		}

		return isLoggedIn;
	}
}
