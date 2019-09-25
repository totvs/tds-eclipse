package br.com.totvs.tds.ui.server.views;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.NotEnabledException;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.commands.ParameterizedCommand;
import org.eclipse.core.commands.common.NotDefinedException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.services.IServiceLocator;

import br.com.totvs.tds.server.ServerActivator;
import br.com.totvs.tds.server.interfaces.IAppServerInfo;
import br.com.totvs.tds.server.interfaces.IServerInfo;
import br.com.totvs.tds.server.interfaces.IServerManager;
import br.com.totvs.tds.ui.server.ServerUIActivator;
import br.com.totvs.tds.ui.server.nl.Messages;

/**
 * Classe para restaurar o estado dos servidores.
 *
 * @author eriky.kashivagui
 * @author acandido
 *
 *
 */
public class ServerStateLoaderJob extends Job {

	private static final String NOT_FOUND = Messages.ServerStateLoaderJob_server_not_found;

	private final String currentServer;
	private final List<String> targetServers;

	/**
	 * Construtor.
	 */
	public ServerStateLoaderJob(List<String> targetServers, String currentServer) {
		super(Messages.ServerStateLoaderJob_server_load_job);

		this.targetServers = targetServers;
		this.currentServer = currentServer;

		setUser(false);
		setSystem(true);
	}

	/**
	 * Efetua a conexão e login autom�tico.
	 *
	 * @param status      lista para adição de ocorr�ncias.
	 * @param serverInfo, servidor alvo
	 * @throws NotHandledException
	 * @throws NotEnabledException
	 * @throws NotDefinedException @
	 */
	private boolean connectAndLogin(MultiStatus status, IServerInfo serverInfo)
			throws ExecutionException, NotDefinedException, NotEnabledException, NotHandledException {
		IServiceLocator serviceLocator = PlatformUI.getWorkbench();
		ICommandService commandService = serviceLocator.getService(ICommandService.class);

		Command command = commandService.getCommand("br.com.totvs.tds.ui.server.command.login"); //$NON-NLS-1$

		Map<String, Object> parameters = new HashMap<String, Object>();
		System.out.println("ServerStateLoaderJob.connectAndLogin()"); //$NON-NLS-1$
//		if (serverInfo.getServerType().equals(ServerType.PROTHEUS.type)) {
//			parameters.put("br.com.totvs.tds.ui.server.command.loginDialog", //$NON-NLS-1$
//					"br.com.totvs.tds.ui.server.tools.ProtheusLoginDialog"); //$NON-NLS-1$
//		} else {
//			parameters.put("br.com.totvs.tds.ui.server.command.loginDialog", //$NON-NLS-1$
//					"br.com.totvs.tds.ui.server.tools.LogixLoginDialog"); //$NON-NLS-1$
//		}
		parameters.put("br.com.totvs.tds.ui.server.command.login.server", serverInfo.getName()); //$NON-NLS-1$
		parameters.put("br.com.totvs.tds.ui.server.command.login.forceLogin", "false"); //$NON-NLS-1$ //$NON-NLS-2$

		ParameterizedCommand pc = ParameterizedCommand.generateCommand(command, parameters);

		IHandlerService handlerService = serviceLocator.getService(IHandlerService.class);
		boolean logged = (boolean) handlerService.executeCommand(pc, null);

		return logged;
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		IServerManager serverManager = ServerActivator.getDefault().getServerManager();
		MultiStatus status = new MultiStatus(ServerUIActivator.PLUGIN_ID, 0,
				Messages.ServerStateLoaderJob_reconnection_server, null);

		if (targetServers != null) {
			for (String name : targetServers) {
				IServerInfo serverInfo = serverManager.getServer(name);

				if (serverInfo != null) {
					try {
						connectAndLogin(status, serverInfo);
					} catch (ExecutionException e) {
						status.add(new Status(IStatus.WARNING, ServerUIActivator.PLUGIN_ID,
								String.format(Messages.ServerStateLoaderJob_server_identifier, e.getMessage(), name),
								e));
						e.printStackTrace();
					} catch (NotDefinedException e) {
						status.add(new Status(IStatus.WARNING, ServerUIActivator.PLUGIN_ID,
								String.format(Messages.ServerStateLoaderJob_server_identifier, e.getMessage(), name),
								e));
						e.printStackTrace();
					} catch (NotEnabledException e) {
						status.add(new Status(IStatus.WARNING, ServerUIActivator.PLUGIN_ID,
								String.format(Messages.ServerStateLoaderJob_server_identifier, e.getMessage(), name),
								e));
						e.printStackTrace();
					} catch (NotHandledException e) {
						status.add(new Status(IStatus.WARNING, ServerUIActivator.PLUGIN_ID,
								String.format(Messages.ServerStateLoaderJob_server_identifier, e.getMessage(), name),
								e));
						e.printStackTrace();
					}
				} else {
					status.add(new Status(IStatus.WARNING, ServerUIActivator.PLUGIN_ID,
							String.format(Messages.ServerStateLoaderJob_reconect_error, NOT_FOUND, name)));
				}
			}
		}

		if ((currentServer != null) && !currentServer.isEmpty()) {
			IServerInfo serverInfo = serverManager.getServer(currentServer);

			if (serverInfo != null) {
				serverManager.setCurrentServer((IAppServerInfo) serverInfo);
			} else {
				status.add(new Status(IStatus.WARNING, ServerUIActivator.PLUGIN_ID,
						String.format(Messages.ServerStateLoaderJob_server_identifier, NOT_FOUND, currentServer)));
			}
		}

		return status.getChildren().length == 0 ? Status.OK_STATUS : status;
	}
}
